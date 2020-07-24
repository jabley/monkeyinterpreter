package object

// NewEnclosedEnvironment creates a new Environment with a reference to the Environment that it extends.
// This preserves existing bindings whilst still making new ones available.
func NewEnclosedEnvironment(outer *Environment) *Environment {
	env := NewEnvironment()
	env.outer = outer
	return env
}

// NewEnvironment creates an Environment ready for use.
func NewEnvironment() *Environment {
	return &Environment{store: make(map[string]Object)}
}

// Environment is an object to contain context for scopes.
type Environment struct {
	store map[string]Object
	outer *Environment
}

// Get returns the named object and a bool indicating whether it is present or not
func (e *Environment) Get(name string) (Object, bool) {
	obj, ok := e.store[name]
	if !ok && e.outer != nil {
		obj, ok = e.outer.Get(name)
	}
	return obj, ok
}

// Set sets the named value in this Environment.
func (e *Environment) Set(name string, val Object) Object {
	e.store[name] = val
	return val
}
