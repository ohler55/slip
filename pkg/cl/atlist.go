// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// atList of Objects.
type atList []slip.Object

// String representation of the Object.
func (obj atList) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj atList) Append(b []byte) []byte {
	return slip.List(obj).Append(b)
}

// Simplify the Object into a []interface{}.
func (obj atList) Simplify() interface{} {
	return nil
}

// Equal returns true if this Object and the other are equal in value.
func (obj atList) Equal(_ slip.Object) (eq bool) {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj atList) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.TrueSymbol}
}

// Eval panics unless the atList is empty.
func (obj atList) Eval(_ *slip.Scope, _ int) slip.Object {
	return obj
}

// LoadForm returns a form that can be evaluated to create the object.
func (obj atList) LoadForm() slip.Object {
	return obj
}
