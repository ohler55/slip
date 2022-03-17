// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"time"
)

// TimeSymbol is the symbol with a value of "time".
const TimeSymbol = Symbol("time")

func init() {
	DefConstant(TimeSymbol, TimeSymbol,
		`A _time_ identifies an instant in time backed by a golang time.Time.`)
}

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

// Simplify the Object into a time.Time.
func (obj Time) Simplify() interface{} {
	return time.Time(obj)
}

// Equal returns true if this Object and the other are equal in value.
func (obj Time) Equal(other Object) (eq bool) {
	if to, ok := other.(Time); ok {
		return time.Time(obj).Equal(time.Time(to))
	}
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj Time) Hierarchy() []Symbol {
	return []Symbol{TimeSymbol, TrueSymbol}
}

// Eval returns self.
func (obj Time) Eval(s *Scope, depth int) Object {
	return obj
}
