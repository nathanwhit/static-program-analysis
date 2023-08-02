mod impls;

use std::fmt::Debug;

use common::never::{Never, ResultNeverExt};

use crate::{AstCtx, Exp, Fun, Id, Program, Stm};

pub trait Foldable: Sized + Clone + Debug {
    fn try_fold_with<F: FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as FallibleFolder>::Error>;
    fn fold_with<F: Folder>(self, ctx: &mut AstCtx, folder: &mut F) -> Self {
        self.try_fold_with(ctx, folder).unwrap_never()
    }
}

pub trait SuperFoldable: Foldable {
    fn try_super_fold_with<F: FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as FallibleFolder>::Error>;

    fn super_fold_with<F: Folder>(self, ctx: &mut AstCtx, folder: &mut F) -> Self {
        self.try_super_fold_with(ctx, folder).unwrap_never()
    }
}

pub trait FallibleFolder: Sized {
    type Error: Debug;
    fn try_fold_exp(&mut self, ctx: &mut AstCtx, exp: Exp) -> Result<Exp, Self::Error> {
        exp.try_super_fold_with(ctx, self)
    }

    fn try_fold_stm(&mut self, ctx: &mut AstCtx, stm: Stm) -> Result<Stm, Self::Error> {
        stm.try_super_fold_with(ctx, self)
    }

    fn try_fold_fun(&mut self, ctx: &mut AstCtx, fun: Fun) -> Result<Fun, Self::Error> {
        fun.try_super_fold_with(ctx, self)
    }

    fn try_fold_id(&mut self, _ctx: &mut AstCtx, id: Id) -> Result<Id, Self::Error> {
        Ok(id)
    }

    fn try_fold_program(
        &mut self,
        ctx: &mut AstCtx,
        program: Program,
    ) -> Result<Program, Self::Error> {
        program.try_super_fold_with(ctx, self)
    }
}

pub trait Folder: FallibleFolder<Error = Never> {
    fn fold_exp(&mut self, ctx: &mut AstCtx, exp: Exp) -> Exp {
        exp.super_fold_with(ctx, self)
    }

    fn fold_stm(&mut self, ctx: &mut AstCtx, stm: Stm) -> Stm {
        stm.super_fold_with(ctx, self)
    }

    fn fold_fun(&mut self, ctx: &mut AstCtx, fun: Fun) -> Fun {
        fun.super_fold_with(ctx, self)
    }

    fn fold_id(&mut self, _ctx: &mut AstCtx, id: Id) -> Id {
        id
    }

    fn fold_program(&mut self, ctx: &mut AstCtx, program: Program) -> Program {
        program.super_fold_with(ctx, self)
    }
}

impl<F: Folder> FallibleFolder for F {
    type Error = Never;

    fn try_fold_exp(&mut self, ctx: &mut AstCtx, exp: Exp) -> Result<Exp, Self::Error> {
        Ok(self.fold_exp(ctx, exp))
    }

    fn try_fold_fun(&mut self, ctx: &mut AstCtx, fun: Fun) -> Result<Fun, Self::Error> {
        Ok(self.fold_fun(ctx, fun))
    }

    fn try_fold_stm(&mut self, ctx: &mut AstCtx, stm: Stm) -> Result<Stm, Self::Error> {
        Ok(self.fold_stm(ctx, stm))
    }

    fn try_fold_id(&mut self, ctx: &mut AstCtx, id: Id) -> Result<Id, Self::Error> {
        Ok(self.fold_id(ctx, id))
    }

    fn try_fold_program(
        &mut self,
        ctx: &mut AstCtx,
        program: Program,
    ) -> Result<Program, Self::Error> {
        Ok(self.fold_program(ctx, program))
    }
}
