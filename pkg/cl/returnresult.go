// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

// ReturnResult is returned by the return-from function.
type ReturnResult struct {
	// Tag can be either a Symbol or nil and identifies the block name that
	// the results should returned from.
	Tag slip.Object
	// Result to return.
	Result slip.Object
}

// String returns a string representation of the object.
func (rr *ReturnResult) String() string {
	return string(rr.Append(nil))
}

// Append the object to a byte slice.
func (rr *ReturnResult) Append(b []byte) []byte {
	b = append(b, "#<return-result "...)
	b = slip.ObjectAppend(b, rr.Tag)
	return append(b, '>')
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (rr *ReturnResult) Simplify() any {
	return map[string]any{
		"tag":    slip.Simplify(rr.Tag),
		"result": slip.Simplify(rr.Result),
	}
}

// Equal returns true if this Object and the other are equal in value.
func (rr *ReturnResult) Equal(other slip.Object) bool {
	return rr == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (rr *ReturnResult) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.TrueSymbol}
}

// Eval the object.
func (rr *ReturnResult) Eval(s *slip.Scope, depth int) slip.Object {
	return rr
}
