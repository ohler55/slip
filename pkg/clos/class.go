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

// Class is a CLOS class.
type Class struct {
	name         string
	docs         string
	inherit      []*Class // direct supers
	prototype    slip.Object
	final        bool
	noMake       bool
	slots        map[string]slip.Object
	InstanceInit func(inst slip.Instance, obj slip.Object)
}

// DefClass creates a Class.
func DefClass(
	name, docs string,
	slots map[string]slip.Object,
	supers []*Class,
	final bool) (class *Class) {

	name = strings.ToLower(name)
	if slip.FindClass(name) != nil {
		slip.NewPanic("Class %s already defined.", name)
	}
	class = &Class{
		name:    name,
		docs:    docs,
		slots:   slots,
		inherit: supers,
		final:   final,
	}
	var hasSO bool
	for _, c := range class.inherit {
		if c == &standardObjectClass {
			hasSO = true
			break
		}
	}
	if !hasSO {
		class.inherit = append(class.inherit, &standardObjectClass)
	}
	class.mergeInherited()
	slip.RegisterClass(name, class)
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
	return map[string]any{
		"name":      c.name,
		"docs":      c.docs,
		"inherit":   clist,
		"final":     c.final,
		"prototype": slip.Simplify(c.prototype),
		"slots":     slots,
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

// Inherits returns true if this Class inherits from a specified Class.
func (c *Class) Inherits(sc slip.Class) bool {
	for _, c2 := range c.inherit {
		if c2 == sc {
			return true
		}
	}
	return false
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

// SetDocumentation of the class.
func (c *Class) SetDocumentation(doc string) {
	c.docs = doc
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

// SetNoMake sets the noMake field.
func (c *Class) SetNoMake(noMake bool) {
	c.noMake = noMake
}

// MakeInstance creates a new instance but does not call the :init method.
func (c *Class) MakeInstance() slip.Instance {
	inst := flavors.Instance{Type: c}
	inst.Vars = map[string]slip.Object{}
	for k, v := range c.slots {
		inst.Vars[k] = v
	}
	inst.Vars["self"] = &inst

	return &inst
}

// InvokeMethod on an object. A temporary flavors.Instance is created and the
// method is invoked on that instance.
func (c *Class) InvokeMethod(obj slip.Object, s *slip.Scope, message string, args slip.List, depth int) slip.Object {
	inst := c.MakeInstance()
	if c.InstanceInit != nil {
		c.InstanceInit(inst, obj)
	}
	return inst.Receive(s, message, args, depth)
}

func (c *Class) mergeInherited() {
	if c.slots == nil {
		c.slots = map[string]slip.Object{}
	}
	var iif func(inst slip.Instance, obj slip.Object)
	for i := len(c.inherit) - 1; 0 <= i; i-- {
		ic := c.inherit[i]
		if ic.InstanceInit != nil {
			iif = ic.InstanceInit
		}
		for k, v := range ic.slots {
			c.slots[k] = v
		}
	}
	if c.InstanceInit == nil {
		c.InstanceInit = iif
	}
}

// DefList returns a list that can be evaluated to create the class or nil if
// the class is a built in class.
func (c *Class) DefList() slip.List {
	// There is no defclass currently.
	return nil
}
