// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// ListSymbol is the symbol with a value of "list".
const ListSymbol = Symbol("list")

func init() {
	DefConstant(ListSymbol, ListSymbol, `A _cons_ is a sequence of _objects_.`)
}

// List of Objects.
type List []Object

// String representation of the Object.
func (obj List) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj List) Append(b []byte) []byte {
	b = append(b, '(')
	for i := len(obj) - 1; 0 <= i; i-- {
		v := obj[i]
		if v == nil {
			b = append(b, "nil"...)
		} else {
			b = v.Append(b)
		}
		if 0 < i {
			b = append(b, ' ')
		}
	}
	return append(b, ')')
}

// Simplify the Object into a []interface{}.
func (obj List) Simplify() interface{} {
	out := make([]interface{}, 0, len(obj))
	for i := len(obj) - 1; 0 <= i; i-- {
		o := obj[i]
		if o == nil {
			out = append(out, nil)
		} else {
			out = append(out, o.Simplify())
		}
	}
	return out
}

// Equal returns true if this Object and the other are equal in value.
func (obj List) Equal(other Object) (eq bool) {
	switch to := other.(type) {
	case List:
		if len(obj) == len(to) {
			eq = true
			for i, co := range obj {
				if !ObjectEqual(co, to[i]) {
					eq = false
					break
				}
			}
		}
	case Cons:
		if len(obj) == len(to) {
			eq = true
			for i, co := range obj {
				if !ObjectEqual(co, to[i]) {
					eq = false
					break
				}
			}
		}
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj List) Hierarchy() []Symbol {
	return []Symbol{ListSymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType returns 'list.
func (obj List) SequenceType() Symbol {
	return ListSymbol
}
