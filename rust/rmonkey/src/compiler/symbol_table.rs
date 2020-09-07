use crate::object::builtins;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub enum SymbolScope {
    Global,
    Local,
    BuiltIn,
    Free,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

/// SymbolTable is a https://en.wikipedia.org/wiki/Symbol_table
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
    pub free_symbols: Vec<Symbol>,
    pub outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new_with_builtins() -> Self {
        let mut res = Self::default();

        for (i, b) in builtins::BUILTINS.iter().enumerate() {
            res.define_builtin(i, b.name);
        }

        res
    }

    pub fn new_enclosed(outer: &SymbolTable) -> Self {
        let mut res = Self::default();

        res.outer = Some(Rc::new(RefCell::new(outer.clone())));

        res
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let s = Symbol {
            name: name.to_owned(),
            scope: match self.outer {
                Some(_) => SymbolScope::Local,
                None => SymbolScope::Global,
            },
            index: self.num_definitions,
        };

        self.store.insert(name.to_string(), s.clone());
        self.num_definitions += 1;

        s
    }

    fn define_builtin(&mut self, index: usize, name: &str) -> Symbol {
        let s = Symbol {
            name: name.to_owned(),
            scope: SymbolScope::BuiltIn,
            index,
        };

        self.store.insert(name.to_string(), s.clone());

        s
    }

    fn define_free(&mut self, original: Symbol) -> Symbol {
        let s = Symbol {
            name: original.name.to_string(),
            index: self.free_symbols.len(),
            scope: SymbolScope::Free,
        };

        self.store.insert(s.name.to_string(), s.clone());

        self.free_symbols.push(original);

        s
    }

    pub fn resolve(&mut self, name: &str) -> Option<Symbol> {
        let (res, from_outer) = self.resolve_internal(name);

        if let Some(original) = res.clone() {
            if from_outer && ![SymbolScope::BuiltIn, SymbolScope::Global].contains(&original.scope)
            {
                return Some(self.define_free(original.clone()));
            }
        }

        res
    }

    fn resolve_internal(&self, name: &str) -> (Option<Symbol>, bool) {
        let mut from_outer = false;

        let res = match self.store.get(&name.to_string()) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(symbol_table) => {
                    from_outer = true;
                    symbol_table.as_ref().borrow_mut().resolve(name)
                }

                None => None,
            },
        };

        (res, from_outer)
    }

    pub fn num_definitions(&self) -> usize {
        self.num_definitions
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            store: Default::default(),
            num_definitions: 0,
            free_symbols: vec![],
            outer: None,
        }
    }
}

#[cfg(test)]
mod tests {

    use super::{Symbol, SymbolScope, SymbolTable};
    use std::collections::HashMap;
    #[test]
    fn define() {
        let expected: HashMap<_, _> = vec![
            (
                "a",
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "d",
                Symbol {
                    name: "d".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
            (
                "e",
                Symbol {
                    name: "e".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "f",
                Symbol {
                    name: "f".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
        ]
        .into_iter()
        .collect();

        let mut global = SymbolTable::default();

        let a = global.define("a");
        assert_eq!(*expected.get("a").unwrap(), a);

        let b = global.define("b");
        assert_eq!(*expected.get("b").unwrap(), b);

        let mut first_local = SymbolTable::new_enclosed(&global);

        let c = first_local.define("c");
        assert_eq!(*expected.get("c").unwrap(), c);

        let d = first_local.define("d");
        assert_eq!(*expected.get("d").unwrap(), d);

        let mut second_local = SymbolTable::new_enclosed(&first_local);
        let e = second_local.define("e");
        assert_eq!(*expected.get("e").unwrap(), e);

        let f = second_local.define("f");
        assert_eq!(*expected.get("f").unwrap(), f);
    }

    #[test]
    fn resolve() {
        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let expected: HashMap<_, _> = vec![
            (
                "a",
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
        ]
        .into_iter()
        .collect();

        for (_, v) in expected {
            assert_symbol_is_resolvable(&mut global, v);
        }
    }

    #[test]
    fn resolve_local() {
        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(&global);
        local.define("c");
        local.define("d");

        let expected: HashMap<_, _> = vec![
            (
                "a",
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
            ),
            (
                "d",
                Symbol {
                    name: "d".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ),
        ]
        .into_iter()
        .collect();

        for (_, v) in expected {
            assert_symbol_is_resolvable(&mut local, v);
        }
    }

    fn assert_symbol_is_resolvable(symbol_table: &mut SymbolTable, expected: Symbol) {
        if let Some(actual) = symbol_table.resolve(&expected.name) {
            assert_eq!(expected, actual)
        } else {
            assert!(false, "name {} is not resolvable", expected.name)
        }
    }

    #[test]
    fn resolve_nested_local() {
        let check_symbols = |symbol_table: &mut SymbolTable, expected_symbols: Vec<Symbol>| {
            for expected in expected_symbols {
                assert_symbol_is_resolvable(symbol_table, expected);
            }
        };

        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(&global);
        first_local.define("c");
        first_local.define("d");

        // slightly cheating here, in asserting against the first_local value before moving on to
        // the second_local. This makes the borrow-checker happy, but we might have to address this
        // design later on.

        check_symbols(
            &mut first_local,
            vec![
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "c".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "d".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        );

        let mut second_local = SymbolTable::new_enclosed(&first_local);
        second_local.define("e");
        second_local.define("f");

        check_symbols(
            &mut second_local,
            vec![
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "e".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "f".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        );
    }

    #[test]
    fn define_resolve_builtins() {
        let mut global = SymbolTable::default();

        let expected = vec![
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::BuiltIn,
                index: 0,
            },
            Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::BuiltIn,
                index: 1,
            },
            Symbol {
                name: "e".to_owned(),
                scope: SymbolScope::BuiltIn,
                index: 2,
            },
            Symbol {
                name: "f".to_owned(),
                scope: SymbolScope::BuiltIn,
                index: 3,
            },
        ];

        for s in &expected {
            global.define_builtin(s.index, &s.name);
        }

        let assert_builtins = |symbol_table: &mut SymbolTable, expected: &Vec<Symbol>| {
            for s in expected {
                assert_symbol_is_resolvable(symbol_table, s.clone());
            }
        };

        assert_builtins(&mut global.clone(), &expected);

        let first_local = SymbolTable::new_enclosed(&global);

        assert_builtins(&mut first_local.clone(), &expected);

        let second_local = SymbolTable::new_enclosed(&first_local);

        assert_builtins(&mut second_local.clone(), &expected);
    }

    #[test]
    fn resolve_free() {
        let assert_free_symbols = |symbol_table: &mut SymbolTable,
                                   expected_symbols: Vec<Symbol>,
                                   expected_free: Vec<Symbol>| {
            for s in expected_symbols {
                assert_symbol_is_resolvable(symbol_table, s);
            }

            assert_eq!(
                expected_free.len(),
                symbol_table.free_symbols.len(),
                "Wrong number of free symbols"
            );

            for (i, free) in expected_free.iter().enumerate() {
                assert_eq!(free, &symbol_table.free_symbols[i]);
            }
        };

        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(&global);
        first_local.define("c");
        first_local.define("d");

        assert_free_symbols(
            &mut first_local,
            vec![
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "c".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "d".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
            vec![],
        );

        let mut second_local = SymbolTable::new_enclosed(&first_local);
        second_local.define("e");
        second_local.define("f");

        assert_free_symbols(
            &mut second_local,
            vec![
                Symbol {
                    name: "a".to_owned(),
                    scope: SymbolScope::Global,
                    index: 0,
                },
                Symbol {
                    name: "b".to_owned(),
                    scope: SymbolScope::Global,
                    index: 1,
                },
                Symbol {
                    name: "c".to_owned(),
                    scope: SymbolScope::Free,
                    index: 0,
                },
                Symbol {
                    name: "d".to_owned(),
                    scope: SymbolScope::Free,
                    index: 1,
                },
                Symbol {
                    name: "e".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "f".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
            vec![
                Symbol {
                    name: "c".to_owned(),
                    scope: SymbolScope::Local,
                    index: 0,
                },
                Symbol {
                    name: "d".to_owned(),
                    scope: SymbolScope::Local,
                    index: 1,
                },
            ],
        );
    }

    #[test]
    fn resolve_unresolvable_free() {
        let mut global = SymbolTable::default();
        global.define("a");

        let mut first_local = SymbolTable::new_enclosed(&global);
        first_local.define("c");

        let mut second_local = SymbolTable::new_enclosed(&first_local);
        second_local.define("e");
        second_local.define("f");

        for expected in vec![
            Symbol {
                name: "a".to_owned(),
                scope: SymbolScope::Global,
                index: 0,
            },
            Symbol {
                name: "c".to_owned(),
                scope: SymbolScope::Free,
                index: 0,
            },
            Symbol {
                name: "e".to_owned(),
                scope: SymbolScope::Local,
                index: 0,
            },
            Symbol {
                name: "f".to_owned(),
                scope: SymbolScope::Local,
                index: 1,
            },
        ] {
            assert_symbol_is_resolvable(&mut second_local, expected);
        }

        for expected_unresolvable in vec!["b", "d"] {
            assert_eq!(
                None,
                second_local.resolve(expected_unresolvable),
                "{} resolved but was not expected to",
                expected_unresolvable
            );
        }
    }
}
