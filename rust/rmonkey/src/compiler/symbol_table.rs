use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub enum SymbolScope {
    Global,
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
}

impl SymbolTable {
    pub fn define(&mut self, name: &str) -> Symbol {
        let s = Symbol {
            name: name.to_owned(),
            scope: SymbolScope::Global,
            index: self.store.len(),
        };

        self.store.insert(name.to_string(), s.clone());

        s
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        self.store.get(&name.to_string()).cloned()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            store: Default::default(),
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
        ]
        .into_iter()
        .collect();

        let mut global = SymbolTable::default();
        let a = global.define("a");

        assert_eq!(*expected.get("a").unwrap(), a);

        let b = global.define("b");

        assert_eq!(*expected.get("b").unwrap(), b);
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
            if let Some(actual) = global.resolve(&v.name) {
                assert_eq!(v, actual)
            } else {
                assert!(false, "name {} is not resolvable", v.name)
            }
        }
    }
}
