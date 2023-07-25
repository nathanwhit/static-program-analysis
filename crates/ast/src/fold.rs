mod impls;

use std::fmt::Debug;

use common::never::{Never, ResultNeverExt};

use crate::{Exp, Fun, Id, Program, Stm};

pub trait Foldable: Sized + Clone + Debug {
    fn try_fold_with<F: FallibleFolder>(
        self,
        folder: &mut F,
    ) -> Result<Self, <F as FallibleFolder>::Error>;
    fn fold_with<F: Folder>(self, folder: &mut F) -> Self {
        self.try_fold_with(folder).unwrap_never()
    }
}

pub trait SuperFoldable: Foldable {
    fn try_super_fold_with<F: FallibleFolder>(
        self,
        folder: &mut F,
    ) -> Result<Self, <F as FallibleFolder>::Error>;

    fn super_fold_with<F: Folder>(self, folder: &mut F) -> Self {
        self.try_super_fold_with(folder).unwrap_never()
    }
}

pub trait FallibleFolder: Sized {
    type Error: Debug;
    fn try_fold_exp(&mut self, exp: Exp) -> Result<Exp, Self::Error> {
        exp.try_super_fold_with(self)
    }

    fn try_fold_stm(&mut self, stm: Stm) -> Result<Stm, Self::Error> {
        stm.try_super_fold_with(self)
    }

    fn try_fold_fun(&mut self, fun: Fun) -> Result<Fun, Self::Error> {
        fun.try_super_fold_with(self)
    }

    fn try_fold_id(&mut self, id: Id) -> Result<Id, Self::Error> {
        Ok(id)
    }

    fn try_fold_program(&mut self, program: Program) -> Result<Program, Self::Error> {
        program.try_super_fold_with(self)
    }
}

pub trait Folder: FallibleFolder<Error = Never> {
    fn fold_exp(&mut self, exp: Exp) -> Exp {
        exp.super_fold_with(self)
    }

    fn fold_stm(&mut self, stm: Stm) -> Stm {
        stm.super_fold_with(self)
    }

    fn fold_fun(&mut self, fun: Fun) -> Fun {
        fun.super_fold_with(self)
    }

    fn fold_id(&mut self, id: Id) -> Id {
        id
    }

    fn fold_program(&mut self, program: Program) -> Program {
        program.super_fold_with(self)
    }
}

impl<F: Folder> FallibleFolder for F {
    type Error = Never;

    fn try_fold_exp(&mut self, exp: Exp) -> Result<Exp, Self::Error> {
        Ok(self.fold_exp(exp))
    }

    fn try_fold_fun(&mut self, fun: Fun) -> Result<Fun, Self::Error> {
        Ok(self.fold_fun(fun))
    }

    fn try_fold_stm(&mut self, stm: Stm) -> Result<Stm, Self::Error> {
        Ok(self.fold_stm(stm))
    }

    fn try_fold_id(&mut self, id: Id) -> Result<Id, Self::Error> {
        Ok(self.fold_id(id))
    }

    fn try_fold_program(&mut self, program: Program) -> Result<Program, Self::Error> {
        Ok(self.fold_program(program))
    }
}
