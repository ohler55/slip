// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// String is a string Object.
type String string

// String representation of the Object.
func (obj String) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj String) Append(b []byte) []byte {
	b = append(b, '"')
	b = append(b, obj...)
	return append(b, '"')
}
