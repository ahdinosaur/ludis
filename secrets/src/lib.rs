//! Age-encrypted secrets for lusid plans.
//!
//! A lusid project stores secrets as individual `*.age` files under a
//! `secrets/` directory. At apply time the host's age/SSH identity decrypts
//! the subset of files it's a recipient for and hands the plaintexts to
//! `@core/secret` resources by name. Plaintexts never enter the Rimu
//! evaluator — plans reference secrets by name, contents materialise at
//! apply.
//!
//! ## Public surface
//!
//! - [`Secrets`] / [`Secret`] — the decrypted bundle handed to the rest of
//!   the apply pipeline, plus the per-entry plaintext wrapper.
//! - [`Secrets::load`] — one-shot load from `(secrets_dir, identity_path,
//!   guest_mode)` into a ready-to-use bundle, returning [`LoadError`].
//! - [`Redactor`] — substring-scrubs known secret plaintexts out of
//!   per-operation stdout/stderr.
//! - [`reencrypt_for_target`] — host-side re-encryption: scopes the
//!   bundle by `[files]` for the named machine, re-encrypts each file
//!   to a caller-supplied target pubkey. Used by `dev apply` (target =
//!   ephemeral VM keypair) and, eventually, `remote apply` (target =
//!   the machine's own `[machines]` key).
//! - [`cli`] — the `lusid secrets ...` subcommands.
//!
//! Lower-level primitives (identity parsing, `lusid-secrets.toml` loading,
//! per-file encrypt/decrypt, alias matching) are kept crate-private. They're
//! the building blocks for the public surface above; reopen them if a third
//! consumption pattern needs direct access.

mod alias;
mod check;
pub mod cli;
mod crypto;
mod decrypt_all;
mod decrypt_dir;
mod identity;
mod key;
mod load;
mod recipients;
mod redactor;
mod reencrypt;
mod secrets;

pub use crate::load::LoadError;
pub use crate::recipients::RecipientsError;
pub use crate::redactor::Redactor;
pub use crate::reencrypt::{ReencryptForTargetError, ReencryptedSecret, reencrypt_for_target};
pub use crate::secrets::{Secret, Secrets};
