// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// ReturnResult is returned by the return-from function.
type ReturnResult struct {
	// Tag can be either a Symbol or nil and identifies the block name that
	// the results should returned from.
	Tag Object
	// Result to return.
	Result Object
}

// String returns a string representation of the object.
func (rr *ReturnResult) String() string {
	return string(rr.Append(nil))
}

// Append the object to a byte slice.
func (rr *ReturnResult) Append(b []byte) []byte {
	b = append(b, "#<return-result "...)
	b = ObjectAppend(b, rr.Tag)
	return append(b, '>')
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (rr *ReturnResult) Simplify() any {
	return map[string]any{
		"tag":    Simplify(rr.Tag),
		"result": Simplify(rr.Result),
	}
}

// Equal returns true if this Object and the other are equal in value.
func (rr *ReturnResult) Equal(other Object) bool {
	return rr == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (rr *ReturnResult) Hierarchy() []Symbol {
	return []Symbol{TrueSymbol}
}

// Eval the object.
func (rr *ReturnResult) Eval(s *Scope, depth int) Object {
	return rr
}
