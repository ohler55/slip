// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// ConsSymbol is the symbol with a value of "cons".
const ConsSymbol = Symbol("cons")

func init() {
	DefConstant(ConsSymbol, ConsSymbol,
		`A _cons_ is a dotted pair of _objects_ with a _car_ and a _cdr_.`)
}

// Cons of Objects. Basically a List of two Objects but displayed differently.
type Cons []Object

// String representation of the Object.
func (obj Cons) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Cons) Append(b []byte) []byte {
	if len(obj) == 0 {
		return append(b, "nil"...)
	}
	if 2 < len(obj) {
		return (List(obj)).Append(b)
	}
	b = append(b, '(')
	if 1 < len(obj) {
		v := obj[1]
		if v == nil {
			b = append(b, "nil"...)
		} else {
			b = v.Append(b)
		}
		if obj[0] == nil {
			return append(b, ')')
		}
		b = append(b, " . "...)
	}
	v := obj[0]
	if v == nil {
		b = append(b, "nil"...)
	} else {
		b = v.Append(b)
	}
	return append(b, ')')
}

// Car of the Cons.
func (obj Cons) Car() Object {
	if 0 < len(obj) {
		return obj[len(obj)-1]
	}
	return nil
}

// Cdr of the Cons.
func (obj Cons) Cdr() Object {
	switch len(obj) {
	case 0, 1:
		return nil
	case 2:
		return obj[0]
	default:
		return List(obj[:len(obj)-1])
	}
}

// Simplify the Object into a []interface{}.
func (obj Cons) Simplify() interface{} {
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
func (obj Cons) Equal(other Object) (eq bool) {
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
func (obj Cons) Hierarchy() []Symbol {
	return []Symbol{ConsSymbol, ListSymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType returns 'cons.
func (obj Cons) SequenceType() Symbol {
	return ConsSymbol
}
