use std::collections::HashMap;

use ast::{
    fold::{Folder, SuperFoldable},
    Id,
};

struct IdMaps {
    arg_map: HashMap<Id, Id>,
    locals_map: HashMap<Id, Id>,
}

#[derive(Default)]
struct UniqueIdents {
    var_counter: u32,
    fun_counter: u32,
    maps: Option<IdMaps>,
    fun_map: HashMap<Id, Id>,
}

impl UniqueIdents {
    fn new() -> Self {
        Self::default()
    }

    fn new_var(&mut self) -> Id {
        let id = Id::from(format!("var{}", self.var_counter));
        self.var_counter += 1;

        id
    }

    fn new_func(&mut self) -> Id {
        let id = Id::from(format!("fun{}", self.fun_counter));
        self.fun_counter += 1;

        id
    }

    fn normalize(&mut self, ctx: &mut ast::AstCtx, program: ast::Program) -> ast::Program {
        self.fold_program(ctx, program)
    }
}

pub fn normalize_unique_idents(ctx: &mut ast::AstCtx, program: ast::Program) -> ast::Program {
    UniqueIdents::new().normalize(ctx, program)
}

impl Folder for UniqueIdents {
    fn fold_exp(&mut self, ctx: &mut ast::AstCtx, exp: ast::Exp) -> ast::Exp {
        exp.super_fold_with(ctx, self)
    }

    fn fold_stm(&mut self, ctx: &mut ast::AstCtx, stm: ast::Stm) -> ast::Stm {
        stm.super_fold_with(ctx, self)
    }

    fn fold_fun(&mut self, ctx: &mut ast::AstCtx, fun: ast::Fun) -> ast::Fun {
        let arg_map = fun
            .args
            .iter()
            .map(|arg| (arg.clone(), self.new_var()))
            .collect::<HashMap<_, _>>();
        let locals_map = fun
            .locals
            .iter()
            .map(|local| (local.clone(), self.new_var()))
            .collect::<HashMap<_, _>>();
        // this is wasteful
        let new_args = fun
            .args
            .iter()
            .map(|arg| arg_map.get(arg).unwrap().clone())
            .collect::<Vec<_>>();
        let new_locals = fun
            .locals
            .iter()
            .map(|local| locals_map.get(local).unwrap().clone())
            .collect::<Vec<_>>();
        let maps = IdMaps {
            arg_map,
            locals_map,
        };
        self.maps = Some(maps);
        if !fun.is_main() {
            let new_fun = self.new_func();
            self.fun_map.insert(fun.name.clone(), new_fun);
        }
        let new = fun.super_fold_with(ctx, self);

        self.maps = None;
        ast::Fun {
            args: new_args,
            locals: new_locals,
            ..new
        }
    }

    fn fold_id(&mut self, _ctx: &mut ast::AstCtx, id: Id) -> Id {
        if let Some(maps) = &self.maps {
            if let Some(new) = maps.locals_map.get(&id) {
                return new.clone();
            }
            if let Some(new) = maps.arg_map.get(&id) {
                return new.clone();
            }
        }
        if let Some(new) = self.fun_map.get(&id) {
            return new.clone();
        }
        id
    }
}

#[cfg(test)]
mod test {
    use ast::Print;
    use common::trim_indent;
    use pretty_assertions::assert_eq;

    #[test]
    fn unique_idents_works() {
        let input = trim_indent(
            "
                thing(a, b, c) {
                    var d, e, f;
                    d = 1;
                    e = 2;
                    f = d + e;
                    output f;
                    return 0;
                }
                main() {
                    var a, b, c;
                    a = 1;
                    b = 2;
                    c = a + b;
                    output c;
                    return 0;
                }",
        );
        let (mut ast_ctx, parsed) = parse::parse(&input).unwrap();
        let normalized = super::normalize_unique_idents(&mut ast_ctx, parsed);
        let printed = trim_indent(normalized.print(&mut ast_ctx).as_ref());

        assert_eq!(
            trim_indent(
                "
                fun0(var0, var1, var2) {
                    var var3, var4, var5;
                    var3 = 1;
                    var4 = 2;
                    var5 = var3 + var4;
                    output var5;
                    return 0;
                }
                main() {
                    var var6, var7, var8;
                    var6 = 1;
                    var7 = 2;
                    var8 = var6 + var7;
                    output var8;
                    return 0;
                }",
            ),
            printed,
        );
    }
}
