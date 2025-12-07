// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import "io"

// InputStreamSymbol is the symbol with a value of "input-stream".
const InputStreamSymbol = Symbol("input-stream")

// InputStream wraps a io.Reader.
type InputStream struct {
	RuneReader
}

// NewInputStream create and return a new InputStream for the provided
// io.Reader.
func NewInputStream(r io.Reader) *InputStream {
	return &InputStream{RuneReader: RuneReader{Reader: r}}
}

// String representation of the Object.
func (obj *InputStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *InputStream) Append(b []byte) []byte {
	return append(b, "#<INPUT-STREAM>"...)
}

// Simplify the Object into an int64.
func (obj *InputStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *InputStream) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *InputStream) Hierarchy() []Symbol {
	return []Symbol{InputStreamSymbol, StreamSymbol, TrueSymbol}
}

// StreamType returns 'inputStream.
func (obj *InputStream) StreamType() Symbol {
	return InputStreamSymbol
}

// Eval returns self.
func (obj *InputStream) Eval(s *Scope, depth int) Object {
	return obj
}
