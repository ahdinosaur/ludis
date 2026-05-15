//! Match a decryption [`Identity`] back to its alias in a [`Recipients`] config.

use std::path::Path;

use crate::crypto::{decrypt_bytes, encrypt_bytes};
use crate::identity::Identity;
use crate::key::Key;
use crate::recipients::Recipients;

/// Find the alias in `[operators]` or `[machines]` whose key matches
/// `identity`. Implemented as an encrypt-then-decrypt round-trip so it works
/// uniformly across x25519 and SSH without leaking the identity's public
/// material out of the `age` crate.
///
/// Cost is `O(N)` encryptions plus one decryption per table entry until a
/// match is found - `age::Identity` doesn't expose a public-key accessor
/// that's uniform across x25519 and SSH, so the probe is the pragmatic
/// option. Fine for typical team / fleet sizes; worth revisiting if
/// `lusid-secrets.toml` ever grows to hundreds of entries.
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
        let boxed: Vec<Box<dyn age::Recipient + Send>> = match key {
            Key::X25519(k) => vec![Box::new(k.clone())],
            Key::Ssh(k) => vec![Box::new(k.clone())],
        };
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
    use secrecy::ExposeSecret;

    use super::*;

    fn empty_recipients() -> Recipients {
        Recipients {
            operators: IndexMap::new(),
            machines: IndexMap::new(),
            groups: IndexMap::new(),
            files: IndexMap::new(),
        }
    }

    #[test]
    fn x25519() {
        let id = age::x25519::Identity::generate();
        let identity: Identity = id.to_string().expose_secret().parse().unwrap();
        let mut r = empty_recipients();
        r.operators
            .insert("me".to_owned(), Key::X25519(id.to_public()));
        assert_eq!(alias_for_identity(&identity, &r), Some("me"));
    }

    #[test]
    fn no_match() {
        let id_a = age::x25519::Identity::generate();
        let id_b = age::x25519::Identity::generate();
        let identity: Identity = id_b.to_string().expose_secret().parse().unwrap();
        let mut r = empty_recipients();
        r.operators
            .insert("a".to_owned(), Key::X25519(id_a.to_public()));
        assert!(alias_for_identity(&identity, &r).is_none());
    }
}
