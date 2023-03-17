// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// ValuesSymbol is the symbol with a value of "values".
const ValuesSymbol = Symbol("values")

// Values of Objects. Only used for multiple return values.
type Values []Object

// String representation of the Object.
func (obj Values) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Values) Append(b []byte) []byte {
	for i, v := range obj {
		if 0 < i {
			b = append(b, ", "...)
		}
		if v == nil {
			b = append(b, "nil"...)
		} else {
			b = v.Append(b)
		}
	}
	return b
}

// Simplify the Object into a []interface{}.
func (obj Values) Simplify() interface{} {
	out := make([]interface{}, 0, len(obj))
	for _, o := range obj {
		if o == nil {
			out = append(out, nil)
		} else {
			out = append(out, o.Simplify())
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj Values) Equal(other Object) (eq bool) {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Values) Hierarchy() []Symbol {
	return []Symbol{ValuesSymbol, TrueSymbol}
}

// Eval panics.
func (obj Values) Eval(s *Scope, depth int) Object {
	return obj[0]
}

// First value in the multiple values.
func (obj Values) First() Object {
	return obj[0]
}
