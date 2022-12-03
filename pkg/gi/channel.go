// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"strconv"

	"github.com/ohler55/slip"
)

// ChannelSymbol is the symbol with a value of "channel".
const ChannelSymbol = slip.Symbol("channel")

// Channel is a chan of Objects.
type Channel chan slip.Object

// String representation of the Object.
func (obj Channel) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj Channel) Append(b []byte) []byte {
	b = append(b, "#<channel "...)
	b = strconv.AppendInt(b, int64(cap(obj)), 10)
	return append(b, '>')
}

// Simplify by returning the string representation of the flavor.
func (obj Channel) Simplify() interface{} {
	return string(obj.Append([]byte{}))
}

// Equal returns true if this Object and the other are equal in value.
func (obj Channel) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the channel.
func (obj Channel) Hierarchy() []slip.Symbol {
	return []slip.Symbol{ChannelSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj Channel) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}
