// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"
	"fmt"
	"math/rand"
	"strconv"

	"github.com/ohler55/slip"
)

// UUIDSymbol is the symbol with a value of "uuid".
const UUIDSymbol = slip.Symbol("uuid")

// UUID is a pair of uint64 as an array which represents a Version 4 UUID.
type UUID [2]uint64

// String representation of the Object.
func (obj UUID) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj UUID) Append(b []byte) []byte {
	b = append(b, "#<uuid "...)
	b = obj.AppendIETF(b)
	return append(b, '>')
}

// Simplify by returning the string representation of the UUID.
func (obj UUID) Simplify() interface{} {
	return string(obj.AppendIETF([]byte{}))
}

// Equal returns true if this Object and the other are equal in value.
func (obj UUID) Equal(other slip.Object) (eq bool) {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the uuid.
func (obj UUID) Hierarchy() []slip.Symbol {
	return []slip.Symbol{UUIDSymbol, slip.TrueSymbol}
}

// Eval returns self.
func (obj UUID) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// IETF string representation of the UUID.
func (obj UUID) IETF() string {
	return string(obj.AppendIETF([]byte{}))
}

// AppendIETF appends an IETF string representation of the UUID.
func (obj UUID) AppendIETF(b []byte) []byte {
	return fmt.Appendf(b, "%08x-%04x-%04x-%04x-%012x",
		obj[0]>>32,
		(obj[0]>>16)&0x000000000000ffff,
		obj[0]&0x000000000000ffff,
		obj[1]>>48,
		obj[1]&0x0000ffffffffffff)
}

// IsNil returns true if all bytes in the UUID are zero.
func (obj UUID) IsNil() bool {
	return obj[0] == 0 && obj[1] == 0
}

// Bytes representation of the UUID.
func (obj UUID) Bytes() []byte {
	b := make([]byte, 16)
	for i := 0; i < 16; i++ {
		b[i] = byte(obj[i/8] >> (56 - (8 * (i % 8))) & 0x00000000000000FF)
	}
	return b
}

// UUIDParse parses a UUID formatted string and create a new UUID. The
// function is tolerant of the format but will return a zero UUID if parsing
// fails.
func UUIDParse(s string) (u UUID) {
	b := bytes.ReplaceAll([]byte(s), []byte{'-'}, []byte{})
	if len(b) == 32 {
		var err error
		if u[0], err = strconv.ParseUint(string(b[:16]), 16, 64); err == nil {
			u[1], err = strconv.ParseUint(string(b[16:]), 16, 64)
		}
		if err != nil {
			u[0] = 0
			u[1] = 0
		}
	}
	return
}

// NewUUID create a new version 4, a random UUID.
func NewUUID() (u UUID) {
	u[0] = (rand.Uint64() & 0xffffffffffff0fff) | 0x0000000000004000
	u[1] = (rand.Uint64() & 0x3fffffffffffffff) | 0x8000000000000000

	return
}
