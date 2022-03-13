// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"time"
)

// Time is a time.Time Object.
type Time time.Time

// String representation of the Object.
func (a Time) String() string {
	return string(a.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (a Time) Append(b []byte) []byte {
	b = append(b, '@')
	b = time.Time(a).AppendFormat(b, time.RFC3339Nano)
	return b
}
