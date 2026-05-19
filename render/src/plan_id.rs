//! `Render` impl for `lusid-plan::PlanNodeId`.

use crate::display_render;

use lusid_plan::PlanNodeId;

display_render!(PlanNodeId);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Render;
    use lusid_plan::PlanId;
    use std::path::PathBuf;

    #[test]
    fn display_matches_render() {
        let id = PlanNodeId::PlanItem {
            plan_id: PlanId::Path(PathBuf::from("plan.lusid")),
            item_id: "nginx".into(),
        };
        assert_eq!(id.render().to_plain_string(), id.to_string());
    }
}
