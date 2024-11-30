// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"strconv"
	"time"

	"github.com/ohler55/slip"
)

// TimeChannelSymbol is the symbol with a value of "time-channel".
const TimeChannelSymbol = slip.Symbol("time-channel")

// TimeChannel is a chan of time.Time that pops slip.Time Objects.
type TimeChannel <-chan time.Time

// String representation of the Object.
func (obj TimeChannel) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj TimeChannel) Append(b []byte) []byte {
	b = append(b, "#<time-channel "...)
	b = strconv.AppendInt(b, int64(cap(obj)), 10)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj TimeChannel) Simplify() interface{} {
	return string(obj.Append([]byte{}))
}

// Equal returns true if this Object and the other are equal in value.
func (obj TimeChannel) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the channel.
func (obj TimeChannel) Hierarchy() []slip.Symbol {
	return []slip.Symbol{TimeChannelSymbol, ChannelSymbol, slip.TrueSymbol}
}

// Length returns the length of the object.
func (obj TimeChannel) Length() int {
	return len(obj)
}

// Eval returns self.
func (obj TimeChannel) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Pop a value from a channel.
func (obj TimeChannel) Pop() slip.Object {
	return slip.Time((<-obj).UTC())
}
