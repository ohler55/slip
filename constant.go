// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"fmt"
	"strings"
)

var (
	constantValues = map[string]Object{}
	constantDocs   = map[string]string{}
)

// DefConstant defines a constant with a value and documentation. It is
// usually called by an init() function at startup.
func DefConstant(sym Symbol, value Object, doc string) {
	name := strings.ToLower(string(sym))
	if v, has := constantValues[name]; has {
		if ObjectEqual(v, value) { // no change so ignore
			return
		}
		panic(fmt.Sprintf("%s is already defined", sym))
	}
	constantValues[name] = value
	constantDocs[name] = doc
}

// GetConstant gets the value and documentation of the constant bound to the
// symbol.
func GetConstant(sym Symbol) (value Object, doc string, ok bool) {
	name := strings.ToLower(string(sym))
	if value, ok = constantValues[name]; ok {
		doc = constantDocs[name]
	}
	return
}
