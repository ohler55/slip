// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

// Tail is used to indicate the []Object is sons and not a list. The
// difference between a list and a cons in LISP is that the cdr of the last
// cons cell in a list is nil while if the last cdr in a list is a value then
// the list is actually a cons.
type Tail struct {
	Value Object // always non-nil
}

// String representation of the object.
func (t Tail) String() string {
	return string(t.Append(nil))
}

// Append the object to a byte slice.
func (t Tail) Append(b []byte) []byte {
	return t.Value.Append(b)
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (t Tail) Simplify() any {
	return t.Value.Simplify()
}

// Equal returns true if this Object and the other are equal in value.
func (t Tail) Equal(other Object) bool {
	if ot, ok := other.(Tail); ok {
		return t.Value.Equal(ot.Value)
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (t Tail) Hierarchy() []Symbol {
	return t.Value.Hierarchy()
}

// Eval the object.
func (t Tail) Eval(s *Scope, depth int) Object {
	return t.Value.Eval(s, depth)
}
