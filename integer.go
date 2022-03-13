// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
)

// Integer is a int64 Object.
type Integer int64

// String representation of the Object.
func (obj Integer) String() string {
	return strconv.FormatInt(int64(obj), 10)
}

// Append a buffer with a representation of the Object.
func (obj Integer) Append(b []byte) []byte {
	return strconv.AppendInt(b, int64(obj), 10)
}
