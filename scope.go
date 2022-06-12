// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

var beforeEval = noopBefore
var afterEval = normalAfter

type returnFrom struct {
	tag    Object // Symbol or nil
	result Object
}

// Scope encapsulates the scope for a function.
type Scope struct {
	parent     *Scope
	name       Object // can be nil so type can't be Symbol
	Vars       map[string]Object
	returnFrom *returnFrom
}

// NewScope create a new top level Scope.
func NewScope() *Scope {
	return &Scope{
		Vars: map[string]Object{},
	}
}

// NewScope create a new Scope with a parent of the current Scope.
func (s *Scope) NewScope(name Object) *Scope {
	return &Scope{
		name:   name,
		parent: s,
		Vars:   map[string]Object{},
	}
}

// Init a scope.
func (s *Scope) Init() {
	s.Vars = map[string]Object{}
}

// Parent returns the parent scope or nil.
func (s *Scope) Parent() *Scope {
	return s.parent
}

// Let a symbol be bound to the value in this Scope.
func (s *Scope) Let(sym Symbol, value Object) {
	name := strings.ToLower(string(sym))
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if vs, ok := value.(Values); ok {
		value = vs.First()
	}
	s.Vars[name] = value
}

// Get a named variable value.
func (s *Scope) Get(sym Symbol) Object {
	return s.get(strings.ToLower(string(sym)))
}

func (s *Scope) get(name string) Object {
	if value, has := s.Vars[name]; has {
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
	s.set(strings.ToLower(string(sym)), value)
}

func (s *Scope) set(name string, value Object) {
	if _, has := constantValues[name]; has {
		panic(fmt.Sprintf("%s is a constant and thus can't be set", name))
	}
	if _, has := s.Vars[name]; has {
		s.Vars[name] = value
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
	return s.has(strings.ToLower(string(sym)))
}

func (s *Scope) has(name string) bool {
	if _, has := s.Vars[name]; has {
		return true
	}
	if s.parent != nil {
		return s.parent.has(name)
	}
	return CurrentPackage.Has(name)
}

// Remove a variable binding.
func (s *Scope) Remove(sym Symbol) {
	s.remove(strings.ToLower(string(sym)))
}

func (s *Scope) remove(name string) {
	if _, has := s.Vars[name]; has {
		delete(s.Vars, name)
		return
	}
	if s.parent != nil {
		s.parent.remove(name)
	}
}

// Eval evaluates an object and returns the result.
func (s *Scope) Eval(obj Object, depth int) (result Object) {
	if obj != nil {
		result = obj.Eval(s, depth)
	}
	return
}
