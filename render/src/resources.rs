//! `Render` impls for resource family types. Mirrors the structure of
//! `lusid-resource`: per-family modules each implement `Render` for that
//! family's `Params`, `Resource`, `State`, and `Change`, plus the top-level
//! dispatchers in `lusid-resource::lib`.

use crate::display_render;

use lusid_resource::{
    Resource, ResourceChange, ResourceParams, ResourceState,
    apt::{AptChange, AptParams, AptResource, AptState},
    apt_repo::{AptRepoChange, AptRepoParams, AptRepoResource, AptRepoState},
    aur::{AurChange, AurParams, AurResource, AurState},
    command::{CommandChange, CommandParams, CommandResource, CommandState},
    directory::{DirectoryChange, DirectoryParams, DirectoryResource, DirectoryState},
    file::{FileChange, FileParams, FileResource, FileState},
    flatpak::{FlatpakChange, FlatpakParams, FlatpakResource, FlatpakState},
    flatpak_remote::{
        FlatpakRemoteChange, FlatpakRemoteParams, FlatpakRemoteResource, FlatpakRemoteState,
    },
    git::{GitChange, GitParams, GitResource, GitState},
    group::{GroupChange, GroupParams, GroupResource, GroupState},
    pacman::{PacmanChange, PacmanParams, PacmanResource, PacmanState},
    podman::{PodmanChange, PodmanParams, PodmanResource, PodmanState},
    secret::SecretParams,
    systemd::{SystemdChange, SystemdParams, SystemdResource, SystemdState},
    user::{UserChange, UserParams, UserResource, UserState},
};

display_render!(AptParams);
display_render!(AptResource);
display_render!(AptState);
display_render!(AptChange);

display_render!(AptRepoParams);
display_render!(AptRepoResource);
display_render!(AptRepoState);
display_render!(AptRepoChange);

display_render!(AurParams);
display_render!(AurResource);
display_render!(AurState);
display_render!(AurChange);

display_render!(CommandParams);
display_render!(CommandResource);
display_render!(CommandState);
display_render!(CommandChange);

display_render!(DirectoryParams);
display_render!(DirectoryResource);
display_render!(DirectoryState);
display_render!(DirectoryChange);

display_render!(FileParams);
display_render!(FileResource);
display_render!(FileState);
display_render!(FileChange);

display_render!(FlatpakParams);
display_render!(FlatpakResource);
display_render!(FlatpakState);
display_render!(FlatpakChange);

display_render!(FlatpakRemoteParams);
display_render!(FlatpakRemoteResource);
display_render!(FlatpakRemoteState);
display_render!(FlatpakRemoteChange);

display_render!(GitParams);
display_render!(GitResource);
display_render!(GitState);
display_render!(GitChange);

display_render!(GroupParams);
display_render!(GroupResource);
display_render!(GroupState);
display_render!(GroupChange);

display_render!(PacmanParams);
display_render!(PacmanResource);
display_render!(PacmanState);
display_render!(PacmanChange);

display_render!(PodmanParams);
display_render!(PodmanResource);
display_render!(PodmanState);
display_render!(PodmanChange);

display_render!(SecretParams);

display_render!(SystemdParams);
display_render!(SystemdResource);
display_render!(SystemdState);
display_render!(SystemdChange);

display_render!(UserParams);
display_render!(UserResource);
display_render!(UserState);
display_render!(UserChange);

display_render!(ResourceParams);
display_render!(Resource);
display_render!(ResourceState);
display_render!(ResourceChange);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Render;

    /// Spot-check that the generated `Render` impls produce the same text as
    /// the underlying `Display`. One per dispatcher and one nested variant is
    /// enough; the macro path is shared, so per-family coverage would be
    /// repeating the macro test from `lib.rs`.
    #[test]
    fn display_matches_render_for_dispatchers() {
        let params = ResourceParams::Apt(AptParams::Package {
            package: "nginx".into(),
        });
        assert_eq!(params.render().to_plain_string(), params.to_string());

        let change = ResourceChange::Apt(AptChange::Install {
            package: "nginx".into(),
        });
        assert_eq!(change.render().to_plain_string(), change.to_string());
    }

    #[test]
    fn display_matches_render_for_leaf_variants() {
        let params = AptParams::Packages {
            packages: vec!["nginx".into(), "curl".into()],
        };
        assert_eq!(params.render().to_plain_string(), params.to_string());
    }
}
