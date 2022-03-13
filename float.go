// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"strconv"
)

// Float is a int64 Object.
type Float float64

// String representation of the Object.
func (obj Float) String() string {
	return strconv.FormatFloat(float64(obj), 'g', -1, 64)
}

// Append a buffer with a representation of the Object.
func (obj Float) Append(b []byte) []byte {
	return strconv.AppendFloat(b, float64(obj), 'g', -1, 64)
}
