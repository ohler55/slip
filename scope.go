// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
	"sync"
)

// Scope encapsulates the scope for a function.
type Scope struct {
	parent  *Scope
	Name    Object // can be nil so type can't be Symbol
	Vars    map[string]Object
	moo     sync.Mutex
	Block   bool
	TagBody bool
	Macro   bool
	Keep    bool
}

// NewScope create a new top level Scope.
func NewScope() *Scope {
	return &Scope{
		Vars: map[string]Object{},
	}
}

// NewScope create a new Scope with a parent of the current Scope.
func (s *Scope) NewScope() *Scope {
	return &Scope{
		parent:  s,
		Vars:    map[string]Object{},
		Block:   s.Block,
		TagBody: s.TagBody,
		Macro:   s.Macro,
	}
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
	s.moo.Lock()
	if s.Vars == nil {
		s.Vars = map[string]Object{name: value}
	} else {
		s.Vars[name] = value
	}
	s.moo.Unlock()
}

// UnsafeLet a symbol be bound to the value in this Scope. No case conversion
// is performed and no checks are performed.
func (s *Scope) UnsafeLet(sym Symbol, value Object) {
	s.moo.Lock()
	if s.Vars == nil {
		s.Vars = map[string]Object{string(sym): value}
	} else {
		s.Vars[string(sym)] = value
	}
	s.moo.Unlock()
}

// Get a named variable value.
func (s *Scope) Get(sym Symbol) Object {
	return s.get(strings.ToLower(string(sym)))
}

func (s *Scope) get(name string) Object {
	if v, has := constantValues[name]; has {
		return v
	}
	s.moo.Lock()
	if s.Vars != nil {
		if value, has := s.Vars[name]; has {
			s.moo.Unlock()
			return value
		}
	}
	s.moo.Unlock()
	if s.parent != nil {
		return s.parent.get(name)
	}
	if value, has := CurrentPackage.Get(name); has && Unbound != value {
		return value
	}
	panic(NewPanic("Variable %s is unbound.", name))
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
	s.moo.Lock()
	if s.Vars != nil {
		if _, has := s.Vars[name]; has {
			s.Vars[name] = value
			s.moo.Unlock()
			return
		}
	}
	s.moo.Unlock()
	if s.parent != nil {
		s.parent.set(name, value)
		return
	}
	CurrentPackage.Set(name, value)
}

// Has returns true if the variable is exists.
func (s *Scope) Has(sym Symbol) bool {
	return s.has(strings.ToLower(string(sym)))
}

func (s *Scope) has(name string) bool {
	if _, has := constantValues[name]; has {
		return true
	}
	s.moo.Lock()
	if s.Vars != nil {
		if _, has := s.Vars[name]; has {
			s.moo.Unlock()
			return true
		}
	}
	s.moo.Unlock()
	if s.parent != nil {
		return s.parent.has(name)
	}
	return CurrentPackage.Has(name)
}

// Bound returns true if the variable is bound.
func (s *Scope) Bound(sym Symbol) bool {
	return s.bound(strings.ToLower(string(sym)))
}

func (s *Scope) bound(name string) bool {
	if _, has := constantValues[name]; has {
		return true
	}
	s.moo.Lock()
	if s.Vars != nil {
		if _, has := s.Vars[name]; has {
			s.moo.Unlock()
			return true
		}
	}
	s.moo.Unlock()
	if s.parent != nil {
		return s.parent.bound(name)
	}
	if v, has := CurrentPackage.Get(name); has && Unbound != v {
		return true
	}
	return false
}

// Remove a variable binding.
func (s *Scope) Remove(sym Symbol) {
	s.remove(strings.ToLower(string(sym)))
}

func (s *Scope) remove(name string) {
	s.moo.Lock()
	if s.Vars != nil {
		if _, has := s.Vars[name]; has {
			delete(s.Vars, name)
			s.moo.Unlock()
			return
		}
	}
	s.moo.Unlock()
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
