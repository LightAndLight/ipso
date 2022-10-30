use ipso_core::{Builtin, CommonKinds, Declaration, Expr, Type, TypeSig};
use std::rc::Rc;

pub fn decls(common_kinds: &CommonKinds) -> Vec<Rc<Declaration>> {
    vec![
        // run : Cmd -> IO ()
        Rc::new(Declaration::Definition {
            name: Rc::from("run"),
            sig: {
                TypeSig {
                    ty_vars: Vec::new(),
                    body: Type::arrow(
                        common_kinds,
                        Type::Cmd,
                        Type::app(Type::mk_io(common_kinds), Type::Unit),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::Run),
        }),
        // read : Cmd -> IO String
        Rc::new(Declaration::Definition {
            name: Rc::from("read"),
            sig: TypeSig::new(
                vec![],
                Type::arrow(
                    common_kinds,
                    Type::Cmd,
                    Type::app(Type::mk_io(common_kinds), Type::String),
                ),
            ),
            body: Rc::new(Expr::Builtin(Builtin::CmdRead)),
        }),
        // lines : Cmd -> IO (Array String)
        Rc::new(Declaration::Definition {
            name: Rc::from("lines"),
            sig: TypeSig::new(
                vec![],
                Type::arrow(
                    common_kinds,
                    Type::Cmd,
                    Type::app(
                        Type::mk_io(common_kinds),
                        Type::app(Type::mk_array(common_kinds), Type::String),
                    ),
                ),
            ),
            body: Rc::new(Expr::Builtin(Builtin::Lines)),
        }),
        // show : Cmd -> String
        Rc::new(Declaration::Definition {
            name: Rc::from("show"),
            sig: TypeSig::new(vec![], Type::arrow(common_kinds, Type::Cmd, Type::String)),
            body: Rc::new(Expr::Builtin(Builtin::ShowCmd)),
        }),
        // eachline_ : Cmd -> (String -> IO ()) -> IO ()
        Rc::new(Declaration::Definition {
            name: Rc::from("eachline_"),
            sig: {
                TypeSig {
                    ty_vars: vec![],
                    body: Type::arrow(
                        common_kinds,
                        // Cmd
                        Type::Cmd,
                        Type::arrow(
                            common_kinds,
                            // String -> IO ()
                            Type::arrow(
                                common_kinds,
                                Type::String,
                                Type::app(Type::mk_io(common_kinds), Type::Unit),
                            ),
                            // IO ()
                            Type::app(Type::mk_io(common_kinds), Type::Unit),
                        ),
                    ),
                }
            },
            body: Expr::alloc_builtin(Builtin::CmdEachline_),
        }),
    ]
}
