// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

type returnFrom struct {
	tag    Object // Symbol or nil
	result Object
}

// Scope encapsulates the scope for a function.
type Scope struct {
	parent     *Scope
	name       Object // can be nil so type can't be Symbol
	vars       map[string]Object
	returnFrom *returnFrom
	before     func(s *Scope, name string, args List, depth int)
	after      func(s *Scope, name string, args List, depth int)
}

// NewScope create a new top level Scope.
func NewScope() *Scope {
	return &Scope{
		vars:   map[string]Object{},
		before: noopBefore,
		after:  normalAfter,
	}
}

// NewScope create a new Scope with a parent of the current Scope.
func (s *Scope) NewScope(name Object) *Scope {
	return &Scope{
		name:   name,
		parent: s,
		vars:   map[string]Object{},
		before: s.before,
		after:  s.after,
	}
}

// Parent returns the parent scope or nil.
func (s *Scope) Parent() *Scope {
	return s.parent
}

/*
// Before should be called in a go defined Eval function before executing
// other code.
func (s *Scope) Before(obj Object, depth int) {
	s.before(s, obj, depth)
}

// After should be called at the end of a Eval function call using a defer.
func (s *Scope) After(obj Object, depth int) {
	s.after(s, obj, depth)
}
*/

// Trace turns tracing on or off for the scope and any future sub-scopes.
func (s *Scope) Trace(on bool) {
	if on {
		s.before = traceBefore
		s.after = traceAfter
	} else {
		s.before = noopBefore
		s.after = normalAfter
	}
}

// Let a symbol be bound to the value in this Scope.
func (s *Scope) Let(sym Symbol, value Object) {
	name := strings.ToUpper(string(sym))
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if vs, ok := value.(Values); ok {
		value = vs.First()
	}
	s.vars[name] = value
}

// Get a named variable value.
func (s *Scope) Get(sym Symbol) Object {
	return s.get(strings.ToUpper(string(sym)))
}

func (s *Scope) get(name string) Object {
	if value, has := s.vars[name]; has {
		return value
	}
	if s.parent != nil {
		return s.parent.get(name)
	}
	if value, has := CurrentPackage.Get(name); has {
		return value
	}
	panic(fmt.Sprintf("Variable %s is unbound.", name))
}

// Set a variable to the provided value. If sym is bound in this scope the
// binding is changed to the new value. If not then the parent Set() is
// called. If no bindings are found before reaching the World then a new world
// level binding is created.
func (s *Scope) Set(sym Symbol, value Object) {
	if vs, ok := value.(Values); ok {
		value = vs.First()
	}
	s.set(strings.ToUpper(string(sym)), value)
}

func (s *Scope) set(name string, value Object) {
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if _, has := s.vars[name]; has {
		s.vars[name] = value
		return
	}
	if s.parent != nil {
		s.parent.set(name, value)
		return
	}
	CurrentPackage.Set(name, value)
}

// Has returns true if the variable is bound.
func (s *Scope) Has(sym Symbol) bool {
	return s.has(strings.ToUpper(string(sym)))
}

func (s *Scope) has(name string) bool {
	if _, has := s.vars[name]; has {
		return true
	}
	if s.parent != nil {
		return s.parent.has(name)
	}
	return CurrentPackage.Has(name)
}

// Remove a variable binding.
func (s *Scope) Remove(sym Symbol) {
	s.remove(strings.ToUpper(string(sym)))
}

func (s *Scope) remove(name string) {
	if _, has := s.vars[name]; has {
		delete(s.vars, name)
		return
	}
	if s.parent != nil {
		s.parent.remove(name)
	}
}

func noopBefore(s *Scope, name string, args List, depth int) {
}

func normalAfter(s *Scope, name string, args List, depth int) {
	switch tr := recover().(type) {
	case nil:
	case *Panic:
		tr.Stack = append(tr.Stack, ObjectString(append(args, Symbol(name))))
		panic(tr)
	default:
		panic(&Panic{
			Message: fmt.Sprint(tr), Stack: []string{ObjectString(append(args, Symbol(name)))},
		})
	}
}

func traceBefore(s *Scope, name string, args List, depth int) {
	// TBD format trace
}

func traceAfter(s *Scope, name string, args List, depth int) {
	// TBD format trace
	normalAfter(s, name, args, depth)
}

// Eval evaluates an object and returns the result.
func (s *Scope) Eval(obj Object, depth int) (result Object) {
	if obj != nil {
		result = obj.Eval(s, depth)
	}
	return
}
