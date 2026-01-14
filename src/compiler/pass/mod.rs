use crate::ARGS;
use crate::ast::Node;
use crate::report::ReportSender;

mod algebraic;
mod const_fold;
mod dead_branch;
mod dead_code;
mod strength_reduce;
mod type_check;
mod unreachable;
mod var_resolve;

pub use algebraic::AlgebraicPass;
pub use const_fold::ConstFoldPass;
pub use dead_branch::DeadBranchPass;
pub use dead_code::DeadCodePass;
pub use strength_reduce::StrengthReducePass;
pub use type_check::TypeCheckPass;
pub use unreachable::UnreachablePass;
pub use var_resolve::VarResolvePass;

pub trait Pass {
    fn run(&mut self, node: &mut Node);
}

pub fn run_passes(node: &mut Node, reporter: ReportSender) {
    let mut passes: Vec<Box<dyn Pass>> = vec![
        Box::new(VarResolvePass::new(reporter.clone())),
        Box::new(TypeCheckPass::new(reporter.clone())),
    ];

    if !ARGS.no_optimize {
        passes.extend([
            Box::new(ConstFoldPass::new(reporter.clone())) as Box<dyn Pass>,
            Box::new(AlgebraicPass::new(reporter.clone())),
            Box::new(StrengthReducePass::new(reporter.clone())),
            Box::new(DeadBranchPass::new(reporter.clone())),
            Box::new(UnreachablePass::new(reporter.clone())),
            Box::new(DeadCodePass::new(reporter)),
        ]);
    }

    for pass in passes.iter_mut() {
        pass.run(node);
    }
}
