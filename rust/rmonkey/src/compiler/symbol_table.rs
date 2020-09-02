use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(PartialEq, Clone, Debug)]
pub enum SymbolScope {
    Global,
    Local,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Symbol {
    pub name: String,
    pub scope: SymbolScope,
    pub index: usize,
}

/// SymbolTable is a https://en.wikipedia.org/wiki/Symbol_table
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    outer: Option<Rc<RefCell<SymbolTable>>>,
}

impl SymbolTable {
    pub fn new_enclosed(outer: SymbolTable) -> Self {
        let mut res = Self::default();

        res.outer = Some(Rc::new(RefCell::new(outer)));

        res
    }

    pub fn define(&mut self, name: &str) -> Symbol {
        let s = Symbol {
            name: name.to_owned(),
            scope: match self.outer {
                Some(_) => SymbolScope::Local,
                None => SymbolScope::Global,
            },
            index: self.store.len(),
        };

        self.store.insert(name.to_string(), s.clone());

        s
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        match self.store.get(&name.to_string()) {
            Some(value) => Some(value.clone()),
            None => match &self.outer {
                Some(symbol_table) => symbol_table.as_ref().borrow().resolve(name),
                None => None,
            },
        }
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            store: Default::default(),
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

        let mut first_local = SymbolTable::new_enclosed(global);

        let c = first_local.define("c");
        assert_eq!(*expected.get("c").unwrap(), c);

        let d = first_local.define("d");
        assert_eq!(*expected.get("d").unwrap(), d);

        let mut second_local = SymbolTable::new_enclosed(first_local);
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
            assert_symbol_is_resolvable(&global, v);
        }
    }

    #[test]
    fn resolve_local() {
        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let mut local = SymbolTable::new_enclosed(global);
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
            assert_symbol_is_resolvable(&local, v);
        }
    }

    fn assert_symbol_is_resolvable(symbol_table: &SymbolTable, expected: Symbol) {
        if let Some(actual) = symbol_table.resolve(&expected.name) {
            assert_eq!(expected, actual)
        } else {
            assert!(false, "name {} is not resolvable", expected.name)
        }
    }

    #[test]
    fn resolve_nested_local() {
        let check_symbols = |symbol_table: &SymbolTable, expected_symbols: Vec<Symbol>| {
            for expected in expected_symbols {
                assert_symbol_is_resolvable(symbol_table, expected);
            }
        };

        let mut global = SymbolTable::default();
        global.define("a");
        global.define("b");

        let mut first_local = SymbolTable::new_enclosed(global);
        first_local.define("c");
        first_local.define("d");

        // slightly cheating here, in asserting against the first_local value before moving on to
        // the second_local. This makes the borrow-checker happy, but we might have to address this
        // design later on.

        check_symbols(
            &first_local,
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

        let mut second_local = SymbolTable::new_enclosed(first_local);
        second_local.define("e");
        second_local.define("f");

        check_symbols(
            &second_local,
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
    }
}
