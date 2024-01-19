// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
	"strings"
	"sync"
	"unsafe"
)

// Scope encapsulates the scope for a function.
type Scope struct {
	parents []*Scope
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
		parents: []*Scope{s},
		Vars:    map[string]Object{},
		Block:   false,
		TagBody: s.TagBody,
		Macro:   s.Macro,
		Keep:    s.Keep,
	}
}

// Parents returns the parent scope or nil.
func (s *Scope) Parents() []*Scope {
	return s.parents
}

// AddParent adds a parent to the scope.
func (s *Scope) AddParent(p *Scope) {
	s.parents = append(s.parents, p)
}

// InBlock returns true if the scope is a block and has a matching name or one
// of the parents IsBlock() returns true.
func (s *Scope) InBlock(name Object) bool {
	if s.Block && name == s.Name {
		return true
	}
	for _, p := range s.parents {
		if p.InBlock(name) {
			return true
		}
	}
	return false
}

// AllVars returns a map of all the variables in the scope and it's parents.
func (s *Scope) AllVars() map[string]Object {
	all := map[string]Object{}
	for i := len(s.parents) - 1; 0 <= i; i-- {
		for k, v := range s.parents[i].AllVars() {
			all[k] = v
		}
	}
	s.moo.Lock()
	for k, v := range s.Vars {
		all[k] = v
	}
	s.moo.Unlock()
	return all
}

// Let a symbol be bound to the value in this Scope.
func (s *Scope) Let(sym Symbol, value Object) {
	name := strings.ToLower(string(sym))
	if _, has := ConstantValues[name]; has {
		PanicPackage(CurrentPackage, "%s is a constant and thus can't be set", name)
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
	if v, has := ConstantValues[name]; has {
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
	for _, p := range s.parents {
		if value, has := p.localGet(name); has {
			return value
		}
	}
	value, has := CurrentPackage.Get(name)

	if !has || Unbound == value {
		PanicUnboundVariable(Symbol(name), "Variable %s is unbound.", name)
	}
	return value
}

// LocalGet a named variable value.
func (s *Scope) LocalGet(sym Symbol) (Object, bool) {
	return s.localGet(strings.ToLower(string(sym)))
}

func (s *Scope) localGet(name string) (Object, bool) {
	s.moo.Lock()
	if s.Vars != nil {
		if value, has := s.Vars[name]; has {
			s.moo.Unlock()
			return value, true
		}
	}
	s.moo.Unlock()
	for _, p := range s.parents {
		if value, has := p.localGet(name); has {
			return value, true
		}
	}
	return nil, false
}

// Set a variable to the provided value. If sym is bound in this scope the
// binding is changed to the new value. If not then the parent Set() is
// called. If no bindings are found before reaching the World then a new world
// level binding is created.
func (s *Scope) Set(sym Symbol, value Object) {
	if vs, ok := value.(Values); ok {
		value = vs.First()
	}
	if !s.set(strings.ToLower(string(sym)), value) {
		CurrentPackage.Set(string(sym), value)
	}
}

func (s *Scope) set(name string, value Object) bool {
	if _, has := ConstantValues[name]; has {
		PanicPackage(CurrentPackage, "%s is a constant and thus can't be set", name)
	}
	s.moo.Lock()
	if s.Vars != nil {
		if _, has := s.Vars[name]; has {
			s.Vars[name] = value
			s.moo.Unlock()
			return true
		}
	}
	s.moo.Unlock()
	for _, p := range s.parents {
		if has := p.set(name, value); has {
			return true
		}
	}
	return false
}

// Has returns true if the variable is exists.
func (s *Scope) Has(sym Symbol) bool {
	return s.has(strings.ToLower(string(sym)))
}

func (s *Scope) has(name string) bool {
	if _, has := ConstantValues[name]; has {
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
	for _, p := range s.parents {
		if p.has(name) {
			return true
		}
	}
	return CurrentPackage.Has(name)
}

// Bound returns true if the variable is bound.
func (s *Scope) Bound(sym Symbol) bool {
	return s.bound(strings.ToLower(string(sym)))
}

func (s *Scope) bound(name string) bool {
	if _, has := ConstantValues[name]; has {
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
	for _, p := range s.parents {
		if p.bound(name) {
			return true
		}
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
	for _, p := range s.parents {
		p.remove(name)
	}
}

// Eval evaluates an object and returns the result.
func (s *Scope) Eval(obj Object, depth int) (result Object) {
	if obj != nil {
		result = obj.Eval(s, depth)
	}
	return
}

// String returns a string representation of the scope.
func (s *Scope) String() string {
	var b []byte
	b = append(b, "Scope-"...)
	b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(s))), 16)
	b = append(b, ' ', '[')
	for _, p := range s.parents {
		b = strconv.AppendUint(b, uint64(uintptr(unsafe.Pointer(p))), 16)
		b = append(b, ' ')
	}
	b = append(b, ']', '\n')
	for k, v := range s.AllVars() {
		b = append(b, ' ', ' ')
		b = append(b, k...)
		b = append(b, ':', ' ')
		b = ObjectAppend(b, v)
		b = append(b, '\n')
	}
	return string(b)
}
