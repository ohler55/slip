// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

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
