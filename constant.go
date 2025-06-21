// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strings"
)

var (
	// Constants is a map of all defined constants.
	Constants = map[string]*Constant{}
)

type Constant struct {
	Name  string
	Value Object
	Doc   string
	Pkg   *Package
}

// DefConstant defines a constant with a value and documentation. It is
// usually called by an init() function at startup.
func DefConstant(p *Package, name string, value Object, doc string) {
	name = strings.ToLower(name)
	if c, has := Constants[name]; has {
		if ObjectEqual(c.Value, value) { // no change so ignore
			return
		}
		PanicPackage(CurrentPackage, "%s is already defined", name)
	}
	Constants[name] = &Constant{
		Name:  name,
		Value: value,
		Doc:   doc,
		Pkg:   p,
	}
	callSetHooks(p, name)
}

// GetConstant gets the value and documentation of the constant bound to the
// symbol.
func GetConstant(name string) (value Object, doc string, ok bool) {
	name = strings.ToLower(name)
	var c *Constant
	if c, ok = Constants[name]; ok {
		value = c.Value
		doc = c.Doc
	}
	return
}
