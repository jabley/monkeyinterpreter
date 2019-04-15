package compiler

// SymbolScope defines the different scopes for symbols. Strings used for a better debugging experience.
type SymbolScope string

// Define the different types of SymbolScope (more will be added in the future)
const (
	GlobalScope SymbolScope = "GLOBAL"
)

// Symbol is an item in a symbol table. https://en.wikipedia.org/wiki/Symbol_table
type Symbol struct {
	Name  string
	Scope SymbolScope
	Index int
}

// SymbolTable is a https://en.wikipedia.org/wiki/Symbol_table
type SymbolTable struct {
	store          map[string]Symbol
	numDefinitions int
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
	s.store[name] = symbol
	s.numDefinitions++
	return symbol
}

// Resolve returns the named Symbol, or nil and false if there isn't one
func (s *SymbolTable) Resolve(name string) (Symbol, bool) {
	obj, ok := s.store[name]
	return obj, ok
}
