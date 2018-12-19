package object

// NewEnvironment creates an Environment ready for use.
func NewEnvironment() *Environment {
	return &Environment{store: make(map[string]Object)}
}

// Environment is an object to contain context for scopes.
type Environment struct {
	store map[string]Object
}

// Get returns the named object and a bool indicating whether it is present or not
func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	return obj, ok
}

// Set sets the named value in this Environment.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}
