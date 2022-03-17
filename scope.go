// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

// Scope encapsulates the scope for a function.
type Scope struct {
	parent      *Scope
	world       *World
	name        Object // can be nil so can't be Symbol
	vars        map[string]Object
	returnValue Object
	before      func(s *Scope, obj Object, depth int)
	after       func(s *Scope, obj Object, depth int)
}

func (s *Scope) NewScope() *Scope {
	return &Scope{
		parent: s,
		world:  s.world,
		before: noopBefore,
		after:  normalAfter,
	}
}

// Before should be called in a go defined Eval function before executing
// other code.
func (s *Scope) Before(obj Object, depth int) {
	s.before(s, obj, depth)
}

// After should be called at the end of a Eval function call using a defer.
func (s *Scope) After(obj Object, depth int) {
	s.after(s, obj, depth)
}

// World returns the to level World.
func (s *Scope) World() *World {
	return s.world
}

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

// Get a named variable value.
func (s *Scope) Get(sym Symbol) Object {
	return s.get(strings.ToLower(string(sym)))
}

func (s *Scope) get(name string) Object {
	if value, has := s.vars[name]; has {
		return value
	}
	if s.parent != nil {
		return s.parent.get(name)
	}

	// TBD world get, constants and gets by function

	if value, has := s.world.constants[name]; has {
		return value
	}
	if value, has := constantValues[name]; has {
		return value
	}
	panic(fmt.Sprintf("Variable %s is unbound.", name))
}

// Set a variable to the provided value. If sym is bound in this scope the
// binding is changed to the new value. If not then the parent Set() is
// called. If no bindings are found before reaching the World then a new world
// level binding is created.
func (s *Scope) Set(sym Symbol, value Object) {
	s.set(strings.ToLower(string(sym)), value)
}

func (s *Scope) set(name string, value Object) {
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if _, has := s.world.constants[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if _, has := s.vars[name]; has {
		return
	}
	if s.parent != nil {
		s.parent.set(name, value)
		return
	}

	// TBD world set by ptr by function

	s.vars[name] = value
}

// Has returns true if the variable is bound.
func (s *Scope) Has(sym Symbol) bool {
	return s.has(strings.ToLower(string(sym)))
}

func (s *Scope) has(name string) bool {
	if _, has := s.vars[name]; has {
		return true
	}
	if s.parent != nil {
		return s.parent.has(name)
	}

	// TBD world has by ptr by function

	if _, has := s.world.constants[name]; has {
		return true
	}
	if _, has := constantValues[name]; has {
		return true
	}
	return false
}

// Remove a variable binding.
func (s *Scope) Remove(sym Symbol) {
	delete(s.vars, strings.ToLower(string(sym)))
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

func noopBefore(s *Scope, obj Object, depth int) {
}

func normalAfter(s *Scope, obj Object, depth int) {
	switch tr := recover().(type) {
	case nil:
	case *Panic:
		tr.Stack = append(tr.Stack, obj)
		panic(tr)
	default:
		panic(&Panic{Message: fmt.Sprint(tr), Stack: []Object{obj}})
	}
}

func traceBefore(s *Scope, obj Object, depth int) {
	// TBD format trace
}

func traceAfter(s *Scope, obj Object, depth int) {
	// TBD format trace
	switch tr := recover().(type) {
	case nil:
	case *Panic:
		tr.Stack = append(tr.Stack, obj)
		panic(tr)
	default:
		panic(&Panic{Message: fmt.Sprint(tr), Stack: []Object{obj}})
	}
}
