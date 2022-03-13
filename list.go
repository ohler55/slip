// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

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
