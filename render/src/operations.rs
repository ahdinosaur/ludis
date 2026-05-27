//! `Render` impls for operation family types. Mirrors `lusid-operation`'s
//! per-family modules: one `Render` impl per `*Operation` variant plus the
//! top-level `Operation` dispatcher.

use crate::display_render;

use lusid_operation::{
    Operation,
    operations::{
        apt::AptOperation, apt_repo::AptRepoOperation, aur::AurOperation,
        command::CommandOperation, directory::DirectoryOperation, file::FileOperation,
        flatpak::FlatpakOperation, git::GitOperation, group::GroupOperation,
        pacman::PacmanOperation, podman::PodmanOperation, podman_compose::PodmanComposeOperation,
        systemd::SystemdOperation, user::UserOperation,
    },
};

display_render!(AptOperation);
display_render!(AptRepoOperation);
display_render!(AurOperation);
display_render!(CommandOperation);
display_render!(DirectoryOperation);
display_render!(FileOperation);
display_render!(FlatpakOperation);
display_render!(GitOperation);
display_render!(GroupOperation);
display_render!(PacmanOperation);
display_render!(PodmanOperation);
display_render!(PodmanComposeOperation);
display_render!(SystemdOperation);
display_render!(UserOperation);

display_render!(Operation);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Render;
    use lusid_operation::operations::apt::AptOperation;

    #[test]
    fn display_matches_render_for_operation_dispatcher() {
        let op = Operation::Apt(AptOperation::Install {
            packages: vec!["nginx".into()],
        });
        assert_eq!(op.render().to_plain_string(), op.to_string());
    }
}
