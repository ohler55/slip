// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

// Only has one object/value which is True
type boolean bool

// True is the true boolean value.
const True = boolean(true)

// String representation of the Object.
func (obj boolean) String() string {
	return "t"
}

// Append a buffer with a representation of the Object.
func (obj boolean) Append(b []byte) []byte {
	return append(b, 't')
}
