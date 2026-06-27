// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
)

// GetVar get the value bound to the sym argument. It panics if sym is
// unbound.
func GetVar(sym Symbol) (Object, bool) {
	name := strings.ToLower(string(sym))

	return CurrentPackage.Get(name)
}

// SetVar binds the sym argument to a value.
func SetVar(sym Symbol, value Object) {
	CurrentPackage.Set(string(sym), value)
}

// HasVar returns true if the sym argument is bound to a value.
func HasVar(sym Symbol) bool {
	name := strings.ToLower(string(sym))
	CurrentPackage.mu.Lock()
	_, has := CurrentPackage.vars[name]
	CurrentPackage.mu.Unlock()

	return has
}

// RemoveVar removes the binding to the sym argument.
func RemoveVar(sym Symbol) {
	name := strings.ToLower(string(sym))
	CurrentPackage.mu.Lock()
	delete(CurrentPackage.vars, name)
	CurrentPackage.mu.Unlock()
}

// DescribeVar returns the documentation for the variable bound to the sym
// argument.
func DescribeVar(sym Symbol) (doc string) {
	name := strings.ToLower(string(sym))
	CurrentPackage.mu.Lock()
	if vv, has := CurrentPackage.vars[name]; has {
		doc = vv.Doc
	}
	CurrentPackage.mu.Unlock()

	return
}
