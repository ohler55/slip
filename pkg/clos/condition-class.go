// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

// ConditionClassSymbol is the symbol with a value of "condition-class".
const ConditionClassSymbol = slip.Symbol("condition-class")

// ConditionClass is a CLOS condition-class.
type ConditionClass struct {
	StandardClass
}

// String representation of the Object.
func (c *ConditionClass) String() string {
	return string(c.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (c *ConditionClass) Append(b []byte) []byte {
	b = append(b, "#<condition-class "...)
	b = append(b, c.name...)
	return append(b, '>')
}

// Equal returns true if this Object and the other are equal in value.
func (c *ConditionClass) Equal(other slip.Object) (eq bool) {
	return c == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (c *ConditionClass) Hierarchy() []slip.Symbol {
	return []slip.Symbol{ConditionClassSymbol, ClassSymbol, StandardObjectSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (c *ConditionClass) Eval(s *slip.Scope, depth int) slip.Object {
	return c
}

// MakeInstance creates a new instance but does not call the :init method.
func (c *ConditionClass) MakeInstance() slip.Instance {
	if len(c.precedence) == 0 {
		slip.NewPanic("The class %s has undefined superclasses.", c.name)
	}
	obj := StandardObject{
		HasSlots: HasSlots{
			vars:   map[string]slip.Object{},
			locker: slip.NoOpLocker{},
		},
		Type: c,
	}
	c.initObjSlots(&obj)

	return &obj
}

// DefList returns a list that can be evaluated to create the class or nil if
// the class is a built in class.
func (c *ConditionClass) DefList() slip.List {
	def := c.StandardClass.DefList()
	if report := c.vars["report"]; report != nil {
		def = append(def, slip.List{slip.Symbol(":report"), report})
	}
	return def
}

// Metaclass returns the symbol condition-class.
func (c *ConditionClass) Metaclass() slip.Symbol {
	return ConditionClassSymbol
}
