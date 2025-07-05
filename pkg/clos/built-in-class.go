// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

const (
	// BuiltInClassSymbol is the symbol with a value of "built-in-class".
	BuiltInClassSymbol = slip.Symbol("built-in-class")
)

// BuiltInClass is the built-in-class.
type BuiltInClass struct {
	name       string
	docs       string
	inherit    *BuiltInClass // direct super
	prototype  slip.Object
	precedence []slip.Symbol
}

// String representation of the Object.
func (c *BuiltInClass) String() string {
	return string(c.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (c *BuiltInClass) Append(b []byte) []byte {
	b = append(b, "#<built-in-class "...)
	b = append(b, c.name...)
	return append(b, '>')
}

// Simplify by returning the string representation of the class.
func (c *BuiltInClass) Simplify() any {
	simple := map[string]any{
		"name":      c.name,
		"docs":      c.docs,
		"prototype": slip.Simplify(c.prototype),
	}
	if c.inherit != nil {
		simple["inherit"] = c.inherit.name
	}
	return simple
}

// Equal returns true if this Object and the other are equal in value.
func (c *BuiltInClass) Equal(other slip.Object) (eq bool) {
	return c == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (c *BuiltInClass) Hierarchy() []slip.Symbol {
	return []slip.Symbol{BuiltInClassSymbol, ClassSymbol, StandardObjectSymbol, slip.TrueSymbol}
}

// Inherits returns true if this Class inherits from a specified Class.
func (c *BuiltInClass) Inherits(sc slip.Class) bool {
	name := slip.Symbol(sc.Name())
	for _, sym := range c.precedence {
		if name == sym {
			return true
		}
	}
	return false
}

// Eval returns self.
func (c *BuiltInClass) Eval(s *slip.Scope, depth int) slip.Object {
	return c
}

// Name of the class.
func (c *BuiltInClass) Name() string {
	return c.name
}

// Documentation of the class.
func (c *BuiltInClass) Documentation() string {
	return c.docs
}

// SetDocumentation of the class.
func (c *BuiltInClass) SetDocumentation(doc string) {
	c.docs = doc
}

// Describe the class in detail.
func (c *BuiltInClass) Describe(b []byte, indent, right int, ansi bool) []byte {
	b = append(b, indentSpaces[:indent]...)
	if ansi {
		b = append(b, bold...)
		b = append(b, c.name...)
		b = append(b, colorOff...)
	} else {
		b = append(b, c.name...)
	}
	b = append(b, " is a built-in class:\n"...)
	i2 := indent + 2
	i3 := indent + 4
	if 0 < len(c.docs) {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Documentation:\n"...)
		b = slip.AppendDoc(b, c.docs, i3, right, ansi)
		b = append(b, '\n')
	}

	b = append(b, indentSpaces[:i2]...)
	if c.inherit != nil {
		b = append(b, "Direct superclasses:"...)
		b = append(b, ' ')
		b = append(b, c.inherit.name...)
		b = append(b, '\n')
	}

	b = append(b, indentSpaces[:i2]...)
	b = append(b, "Class precedence list:"...)
	for _, sym := range c.precedence {
		b = append(b, ' ')
		b = append(b, sym...)
	}
	b = append(b, '\n')

	if c.prototype != nil {
		b = append(b, indentSpaces[:i2]...)
		b = append(b, "Prototype: "...)
		b = slip.Append(b, c.prototype)
		b = append(b, '\n')
	}
	return b
}

func (c *BuiltInClass) buildPrecedence() {
	for ic := c; ic != nil; ic = ic.inherit {
		c.precedence = append(c.precedence, slip.Symbol(ic.name))
	}
	c.precedence = append(c.precedence, slip.TrueSymbol)
}

// MakeInstance creates a new instance but does not call the :init method.
func (c *BuiltInClass) MakeInstance() slip.Instance {
	panic(slip.NewError("Can not allocate an instance of %s.", c))
}

// DefList returns a list that can be evaluated to create the class or nil if
// the class is a built in class.
func (c *BuiltInClass) DefList() slip.List {
	// There is no defclass currently.
	return nil
}
