//! Parse `.lusid` Rimu source into a [`Plan`] value (with spans preserved for
//! diagnostics).

use std::{cell::RefCell, rc::Rc};

use displaydoc::Display;
use rimu::Spanned;
use rimu_interop::FromRimu;
use thiserror::Error;

use crate::{
    PlanId,
    model::{Plan, PlanFromRimuError},
};

#[derive(Debug, Error, Display)]
pub enum LoadError {
    /// Rimu parse failed: {0:?}
    RimuParse(Vec<rimu::ParseError>),

    /// No code found in source
    NoCode,

    /// Evaluating Rimu AST failed
    RimuEval(#[from] Box<rimu::EvalError>),

    /// Failed to convert Rimu value into Plan
    PlanFromRimu(Box<Spanned<PlanFromRimuError>>),
}

/// Parse Rimu source, evaluate it against an environment seeded with the Rimu stdlib,
/// and project the resulting value into a [`Plan`] (name, version, params schema, setup
/// function).
///
/// The stdlib is in scope at both top-level evaluation and inside the `setup` closure
/// it returns (the closure captures this environment), so plan authors can call
/// `to_string`, `length`, `map`, `range`, `host_path`, `target_path` from any plan
/// expression.
///
/// `plan_id` becomes the Rimu `SourceId` so downstream span-aware errors can point back
/// at the real file.
pub fn load(code: &str, plan_id: &PlanId) -> Result<Spanned<Plan>, LoadError> {
    let source_id = plan_id.clone().into();
    let (ast, errors) = rimu::parse(code, source_id);
    if !errors.is_empty() {
        return Err(LoadError::RimuParse(errors));
    }
    let Some(ast) = ast else {
        return Err(LoadError::NoCode);
    };

    let mut env = rimu::Environment::new();
    for (key, value) in rimu::create_stdlib() {
        env.insert(key, value);
    }
    let env = Rc::new(RefCell::new(env));
    let value = rimu::evaluate(&ast, env).map_err(Box::new)?;
    let plan =
        Plan::from_rimu_spanned(value).map_err(|error| LoadError::PlanFromRimu(Box::new(error)))?;
    Ok(plan)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stdlib_in_scope_at_top_level() {
        let code = r#"
name: "test"
version: to_string(0) + ".1.0"

setup: (params, ctx) => []
"#;
        let plan_id = PlanId::Path("/tmp/test.lusid".into());
        load(code, &plan_id).expect("load should see to_string in scope");
    }
}
