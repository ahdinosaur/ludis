//! Current user info: `$USER` (or `$USERNAME` on Windows), `$HOME`, and the primary
//! group of the running process.
//!
//! Note(cc): trusts env vars for `name`/`home`, which can be unset or spoofed. On Unix,
//! `nix::unistd` already in the workspace would give the real uid → username lookup
//! without needing the env to be set. The `primary_group` lookup already takes that
//! path: `Group::from_gid(getgid())` reads from the kernel-tracked process gid and the
//! NSS group database, neither of which the user's environment can forge.

use std::env;
use std::path::PathBuf;

use nix::unistd::{Group, getgid};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub home: PathBuf,
    pub primary_group: String,
}

#[derive(Error, Debug)]
pub enum GetUserError {
    #[error("missing user")]
    MissingUser,

    #[error("missing home")]
    MissingHome,

    #[error("failed to look up primary group for gid {gid}: {source}")]
    PrimaryGroupLookup {
        gid: u32,
        #[source]
        source: nix::Error,
    },

    #[error("primary group not found for gid {gid}")]
    PrimaryGroupNotFound { gid: u32 },
}

impl User {
    pub fn get() -> Result<Self, GetUserError> {
        let name = get_user().ok_or(GetUserError::MissingUser)?;
        let home = get_home().ok_or(GetUserError::MissingHome)?;
        let primary_group = get_primary_group()?;

        Ok(Self {
            name,
            home,
            primary_group,
        })
    }
}

fn get_home() -> Option<PathBuf> {
    #[cfg(unix)]
    {
        env::var_os("HOME").map(PathBuf::from)
    }

    #[cfg(windows)]
    {
        env::var_os("USERPROFILE").or_else(|| {
            let home_drive = env::var_os("HOMEDRIVE")?;
            let home_path = env::var_os("HOMEPATH")?;
            Some({
                let mut path = PathBuf::from(home_drive);
                path.push(home_path);
                path
            })
        })
    }
}

fn get_user() -> Option<String> {
    #[cfg(unix)]
    {
        env::var("USER").ok()
    }

    #[cfg(windows)]
    {
        env::var("USERNAME").ok()
    }
}

fn get_primary_group() -> Result<String, GetUserError> {
    let gid = getgid();
    let raw_gid = gid.as_raw();
    Group::from_gid(gid)
        .map_err(|source| GetUserError::PrimaryGroupLookup {
            gid: raw_gid,
            source,
        })?
        .map(|group| group.name)
        .ok_or(GetUserError::PrimaryGroupNotFound { gid: raw_gid })
}
