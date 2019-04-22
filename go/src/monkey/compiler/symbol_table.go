package compiler

// SymbolScope defines the different scopes for symbols. Strings used for a better debugging experience.
type SymbolScope string

// Define the different types of SymbolScope (more will be added in the future)
const (
	GlobalScope  SymbolScope = "GLOBAL"
	LocalScope   SymbolScope = "LOCAL"
	BuiltInScope SymbolScope = "BUILTIN"
)

// Symbol is an item in a symbol table. https://en.wikipedia.org/wiki/Symbol_table
type Symbol struct {
	Name  string
	Scope SymbolScope
	Index int
}

// SymbolTable is a https://en.wikipedia.org/wiki/Symbol_table
type SymbolTable struct {
	Outer *SymbolTable

	store          map[string]Symbol
	numDefinitions int
}

// NewEnclosedSymbolTable returns a new SymbolTable with the specified Outer SymbolTable. This is used for nested scopes.
func NewEnclosedSymbolTable(outer *SymbolTable) *SymbolTable {
	s := NewSymbolTable()
	s.Outer = outer
	return s
}

// NewSymbolTable creates a new SymbolTable
func NewSymbolTable() *SymbolTable {
	return &SymbolTable{
		store: make(map[string]Symbol),
	}
}

// Define creates a new Symbol in this SymbolTable
func (s *SymbolTable) Define(name string) Symbol {
	symbol := Symbol{Name: name, Index: s.numDefinitions, Scope: GlobalScope}

	if s.Outer == nil {
		symbol.Scope = GlobalScope
	} else {
		symbol.Scope = LocalScope
	}

	s.store[name] = symbol
	s.numDefinitions++
	return symbol
}

// DefineBuiltIn creates a new Symbol for a built-in function
func (s *SymbolTable) DefineBuiltIn(index int, name string) Symbol {
	symbol := Symbol{Name: name, Index: index, Scope: BuiltInScope}
	s.store[name] = symbol
	return symbol
}

// Resolve returns the named Symbol, or nil and false if there isn't one
func (s *SymbolTable) Resolve(name string) (Symbol, bool) {
	obj, ok := s.store[name]

	if !ok && s.Outer != nil {
		obj, ok = s.Outer.Resolve(name)
	}

	return obj, ok
}
