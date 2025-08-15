// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// GoTo is returned by the return-from function.
type GoTo struct {
	// Tag can be either a Symbol or Integer and identifies a tag in a
	// sequence of statements.
	Tag slip.Object
}

// String returns a string representation of the object.
func (gt *GoTo) String() string {
	return string(gt.Append(nil))
}

// Append the object to a byte slice.
func (gt *GoTo) Append(b []byte) []byte {
	b = append(b, "#<go "...)
	b = slip.ObjectAppend(b, gt.Tag)
	return append(b, '>')
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (gt *GoTo) Simplify() any {
	return map[string]any{
		"tag": slip.Simplify(gt.Tag),
	}
}

// Equal returns true if this Object and the other are equal in value.
func (gt *GoTo) Equal(other slip.Object) bool {
	return gt == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (gt *GoTo) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.TrueSymbol}
}

// Eval the object.
func (gt *GoTo) Eval(s *slip.Scope, depth int) slip.Object {
	return gt
}
