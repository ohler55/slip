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
	if _, has := CurrentPackage.vars[name]; has {
		return true
	}
	return false
}

// RemoveVar removes the binding to the sym argument.
func RemoveVar(sym Symbol) {
	name := strings.ToLower(string(sym))
	delete(CurrentPackage.vars, name)
}

// DescribeVar returns the documentation for the variable bound to the sym
// argument.
func DescribeVar(sym Symbol) string {
	name := strings.ToLower(string(sym))
	if vv, has := CurrentPackage.vars[name]; has {
		return vv.Doc
	}
	return ""
}
