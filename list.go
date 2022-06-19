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
	return printer.Append(b, obj, 0)
}

// Simplify the Object into a []interface{}.
func (obj List) Simplify() interface{} {
	out := make([]interface{}, len(obj))
	for i, o := range obj {
		if o == nil {
			out[len(out)-i-1] = nil
		} else {
			out[len(out)-i-1] = o.Simplify()
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

// Eval panics unless the list is empty.
func (obj List) Eval(s *Scope, depth int) Object {
	if 0 < len(obj) {
		return ListToFunc(s, obj, depth).Eval(s, depth)
	}
	return nil
}
