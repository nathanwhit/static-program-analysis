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
    counter: u32,
    maps: Option<IdMaps>,
}

impl UniqueIdents {
    fn new() -> Self {
        Self::default()
    }

    fn new_var(&mut self) -> Id {
        let id = Id::from(format!("var{}", self.counter));
        self.counter += 1;
        id
    }

    fn normalize(&mut self, program: ast::Program) -> ast::Program {
        self.fold_program(program)
    }
}

pub fn normalize_unique_idents(program: ast::Program) -> ast::Program {
    UniqueIdents::new().normalize(program)
}

impl Folder for UniqueIdents {
    fn fold_exp(&mut self, exp: ast::Exp) -> ast::Exp {
        exp.super_fold_with(self)
    }

    fn fold_stm(&mut self, stm: ast::Stm) -> ast::Stm {
        stm.super_fold_with(self)
    }

    fn fold_fun(&mut self, fun: ast::Fun) -> ast::Fun {
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
        let new = fun.super_fold_with(self);

        self.maps = None;
        ast::Fun {
            args: new_args,
            locals: new_locals,
            ..new
        }
    }

    fn fold_id(&mut self, id: Id) -> Id {
        if let Some(maps) = &self.maps {
            if let Some(new) = maps.arg_map.get(&id) {
                return new.clone();
            }
            if let Some(new) = maps.locals_map.get(&id) {
                return new.clone();
            }
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
        let parsed = parse::parse(&input).unwrap();
        let normalized = super::normalize_unique_idents(parsed);
        let printed = trim_indent(normalized.print().as_ref());

        assert_eq!(
            printed,
            trim_indent(
                "
                thing(var0, var1, var2) {
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
            )
        );
    }
}