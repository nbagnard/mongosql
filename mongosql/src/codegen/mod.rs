mod mql;
use mql::MqlCodeGenerator;
pub use mql::{MqlMappingRegistry, MqlTranslation};

use crate::ir;
use thiserror::Error;

#[cfg(test)]
mod test;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
    #[error("binding tuple key {0:?} not found in mapping registry")]
    ReferenceNotFound(ir::binding_tuple::Key),
    #[error("project fields may not be empty, contain dots, or start with dollars")]
    InvalidProjectField,
    #[error("document keys may not be empty, contain dots, or start with dollars")]
    InvalidDocumentKey,
    #[error("cannot generate MQL for {0:?} function")]
    UnsupportedFunction(ir::ScalarFunction),
    #[error("GROUP BY keys must be valid field references")]
    InvalidGroupKey,
    #[error("sort key must be a field reference")]
    InvalidSortKey,
    #[error("field paths can only be generated for Reference and FieldAccess exprs")]
    NoFieldPathForExpr,
    #[error("limit ({0}) cannot be converted to i64")]
    LimitOutOfI64Range(u64),
    #[error("offset ({0}) cannot be converted to i64")]
    OffsetOutOfI64Range(u64),
}

pub fn generate_mql(plan: ir::Stage) -> Result<MqlTranslation> {
    let cg = MqlCodeGenerator {
        mapping_registry: MqlMappingRegistry::new(),
        scope_level: 0u16,
    };
    cg.codegen_stage(plan)
}
