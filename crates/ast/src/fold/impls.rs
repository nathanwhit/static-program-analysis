use crate::{AstCtx, Exp, Fun, Id, Program, Stm};

use super::Foldable;

// Actual impls

impl Foldable for Id {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        folder.try_fold_id(ctx, self)
    }
}

impl Foldable for Exp {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        folder.try_fold_exp(ctx, self)
    }
}

impl Foldable for Stm {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        folder.try_fold_stm(ctx, self)
    }
}

impl Foldable for Fun {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        folder.try_fold_fun(ctx, self)
    }
}

impl Foldable for Program {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        folder.try_fold_program(ctx, self)
    }
}

// Boring/fundamental impls

impl<T: Foldable> Foldable for Box<T> {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        Ok(Box::new((*self).try_fold_with(ctx, folder)?))
    }
}

impl<T: Foldable> Foldable for Vec<T> {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        self.into_iter()
            .map(|t| t.try_fold_with(ctx, folder))
            .collect()
    }
}

impl<T: Foldable> Foldable for Option<T> {
    fn try_fold_with<F: super::FallibleFolder>(
        self,
        ctx: &mut AstCtx,
        folder: &mut F,
    ) -> Result<Self, <F as super::FallibleFolder>::Error> {
        self.map(|t| t.try_fold_with(ctx, folder)).transpose()
    }
}

macro_rules! impl_foldable_tuple {
    ($($name: ident),+) => {
        impl<$($name),+> Foldable for ($($name),+) where
            $($name: Foldable),+
        {
            paste::paste! {
                fn try_fold_with<F: super::FallibleFolder>(
                    self,
                    ctx: &mut AstCtx,
                    folder: &mut F,
                ) -> Result<Self, <F as super::FallibleFolder>::Error> {
                    let ($( [< $name:lower >] ),+) = self;
                    Ok(($( [< $name:lower >] .try_fold_with(ctx, folder)?),+))
                }
            }
        }
    };
}

impl_foldable_tuple!(A, B);
