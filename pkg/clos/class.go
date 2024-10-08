// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

const (
	// ClassSymbol is the symbol with a value of "class".
	ClassSymbol = slip.Symbol("class")

	// StandardObjectSymbol is the symbol with a value of "standard-object".
	StandardObjectSymbol = slip.Symbol("standard-object")

	bold         = "\x1b[1m"
	colorOff     = "\x1b[m"
	indentSpaces = "                                                                                "
)

var allClasses = map[string]*Class{}

// Class is a CLOS class.
type Class struct {
	name      string
	docs      string
	inherit   []*Class // direct supers
	prototype slip.Object
	final     bool
	noMake    bool
	slots     map[string]slip.Object
	methods   map[string][]*flavors.Method
}

// Find finds the named class.
func Find(name string) (c *Class) {
	if c = allClasses[name]; c == nil {
		c = allClasses[strings.ToLower(name)]
	}
	return
}

// DefClass creates a Class.
func DefClass(
	name, docs string,
	slots map[string]slip.Object,
	supers []*Class,
	final bool) (class *Class) {
	name = strings.ToLower(name)
	if _, has := allClasses[name]; has {
		slip.NewPanic("Class %s already defined.", name)
	}
	class = &Class{
		name:    name,
		docs:    docs,
		slots:   slots,
		inherit: supers,
		methods: map[string][]*flavors.Method{},
		final:   final,
	}
	allClasses[name] = class
	return
}

// String representation of the Object.
func (c *Class) String() string {
	return string(c.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (c *Class) Append(b []byte) []byte {
	b = append(b, "#<class "...)
	b = append(b, c.name...)
	return append(b, '>')
}

// Simplify by returning the string representation of the class.
func (c *Class) Simplify() any {
	clist := make([]any, 0, len(c.inherit))
	for _, s := range c.inherit {
		clist = append(clist, s.name)
	}
	slots := map[string]any{}
	for k, o := range c.slots {
		slots[k] = slip.Simplify(o)
	}
	methods := make([]any, 0, len(c.methods))
	for m := range c.methods {
		methods = append(methods, m)
	}
	return map[string]any{
		"name":      c.name,
		"docs":      c.docs,
		"inherit":   clist,
		"final":     c.final,
		"prototype": slip.Simplify(c.prototype),
		"slots":     slots,
		"methods":   methods,
	}
}

// Equal returns true if this Object and the other are equal in value.
func (c *Class) Equal(other slip.Object) (eq bool) {
	return c == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (c *Class) Hierarchy() []slip.Symbol {
	return []slip.Symbol{ClassSymbol, StandardObjectSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (c *Class) Eval(s *slip.Scope, depth int) slip.Object {
	return c
}

// Name of the class.
func (c *Class) Name() string {
	return c.name
}

// Documentation of the class.
func (c *Class) Documentation() string {
	return c.docs
}

func inClassList(c *Class, cpl []*Class) bool {
	for _, s := range cpl {
		if c == s {
			return true
		}
	}
	return false
}

func (c *Class) precedenceList(cpl []*Class) []*Class {
	for _, s := range c.inherit {
		if !inClassList(s, cpl) {
			cpl = append(cpl, s)
			cpl = s.precedenceList(cpl)
		}
	}
	return cpl
}

// Describe the class in detail.
func (c *Class) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = append(b, c.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, c.name...)
	}
	if c.final {
		b = append(b, " is a built-in class:\n"...)
	} else {
		b = append(b, " is a class:\n"...)
	}
	i2 := indent + 2
	i3 := indent + 4
	if 0 < len(c.docs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Documentation:\n"...)
		b = slip.AppendDoc(b, c.docs, i3, right, ansi)
		b = append(b, '\n')
	}

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Direct superclasses:"...)
	for _, f := range c.inherit {
		b = append(b, ' ')
		b = append(b, f.name...)
	}
	b = append(b, '\n')

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Class precedence list:"...)
	for _, f := range c.precedenceList(nil) {
		b = append(b, ' ')
		b = append(b, f.name...)
	}
	b = append(b, '\n')

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Slots:"...)
	if 0 < len(c.slots) {
		b = append(b, '\n')
		var keys []string
		for k := range c.slots {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, " = "...)
			b = slip.Append(b, c.slots[k])
			b = append(b, '\n')
		}
	} else {
		b = append(b, " None\n"...)
	}
	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Methods:"...)
	if 0 < len(c.methods) {
		b = append(b, '\n')
		var keys []string
		for k := range c.methods {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		for _, k := range keys {
			b = append(b, indentSpaces[:i3]...)
			b = append(b, k...)
			b = append(b, '\n')
		}
	} else {
		b = append(b, " None\n"...)
	}
	if c.prototype != nil {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Prototype: "...)
		b = slip.Append(b, c.prototype)
		b = append(b, '\n')
	}
	return b
}

// NoMake returns true if the class does not allows creating a new instance
// with make-instance which should signal an error.
func (c *Class) NoMake() bool {
	return c.noMake
}

// MakeInstance creates a new instance but does not call the :init method.
func (c *Class) MakeInstance() slip.Instance {
	inst := flavors.Instance{Type: c, Methods: c.methods}
	inst.Scope.Vars = map[string]slip.Object{}
	for k, v := range c.slots {
		inst.Vars[k] = v
	}
	inst.Vars["self"] = &inst

	return &inst
}

// DefMethod defines a method for the class. TBD place holder for now.
func (c *Class) DefMethod(name string) {
	c.methods[name] = nil
}
