//! Match a decryption [`Identity`] back to its alias in a [`Recipients`] config.

use std::path::Path;

use crate::crypto::{decrypt_bytes, encrypt_bytes};
use crate::identity::Identity;
use crate::recipients::Recipients;

/// Find the alias in `[operators]` or `[machines]` whose key matches
/// `identity`. Implemented as an encrypt-then-decrypt round-trip so it works
/// uniformly without leaking the identity's public material out of the `age`
/// crate.
///
/// Cost is `O(N)` encryptions plus one decryption per table entry until a
/// match is found - `age::Identity` doesn't expose a public-key accessor, so
/// the probe is the pragmatic option. Fine for typical team / fleet sizes;
/// worth revisiting if `lusid-secrets.toml` ever grows to hundreds of entries.
///
/// Returns `None` when no alias matches; callers should treat this as a hard
/// configuration error (the supplied identity isn't declared anywhere).
pub fn alias_for_identity<'a>(identity: &Identity, recipients: &'a Recipients) -> Option<&'a str> {
    let probe_path = Path::new("__alias_match__");
    for (alias, key) in recipients
        .operators
        .iter()
        .chain(recipients.machines.iter())
    {
        let boxed: Vec<Box<dyn age::Recipient + Send>> = vec![Box::new(key.recipient().clone())];
        let Ok(ct) = encrypt_bytes(&boxed, probe_path, b"") else {
            continue;
        };
        if decrypt_bytes(identity, probe_path, &ct).is_ok() {
            return Some(alias.as_str());
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;

    use super::*;
    use crate::key::Key;
    use crate::test_fixtures::{
        TEST_SSH_ED25519_A_PRIV, TEST_SSH_ED25519_A_PUB, TEST_SSH_ED25519_B_PUB,
    };

    fn empty_recipients() -> Recipients {
        Recipients {
            operators: IndexMap::new(),
            machines: IndexMap::new(),
            groups: IndexMap::new(),
            files: IndexMap::new(),
        }
    }

    #[test]
    fn matches_ssh_operator() {
        let identity: Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let pubkey: Key = TEST_SSH_ED25519_A_PUB.parse().unwrap();
        let mut r = empty_recipients();
        r.operators.insert("me".to_owned(), pubkey);
        assert_eq!(alias_for_identity(&identity, &r), Some("me"));
    }

    #[test]
    fn no_match() {
        // identity is A, the only listed key is B.
        let identity: Identity = TEST_SSH_ED25519_A_PRIV.parse().unwrap();
        let other_pub: Key = TEST_SSH_ED25519_B_PUB.parse().unwrap();
        let mut r = empty_recipients();
        r.operators.insert("a".to_owned(), other_pub);
        assert!(alias_for_identity(&identity, &r).is_none());
    }
}
