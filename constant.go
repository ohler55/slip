// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
)

var (
	// ConstantValues should be treated as read only.
	ConstantValues = map[string]Object{}
	// ConstantDocs should be treated as read only.
	ConstantDocs = map[string]string{}
)

// DefConstant defines a constant with a value and documentation. It is
// usually called by an init() function at startup.
func DefConstant(sym Symbol, value Object, doc string) {
	name := strings.ToLower(string(sym))
	if v, has := ConstantValues[name]; has {
		if ObjectEqual(v, value) { // no change so ignore
			return
		}
		PanicPackage(CurrentPackage, "%s is already defined", sym)
	}
	ConstantValues[name] = value
	ConstantDocs[name] = doc
}

// GetConstant gets the value and documentation of the constant bound to the
// symbol.
func GetConstant(sym Symbol) (value Object, doc string, ok bool) {
	name := strings.ToLower(string(sym))
	if value, ok = ConstantValues[name]; ok {
		doc = ConstantDocs[name]
	}
	return
}
