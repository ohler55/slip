// Copyright (c) 2024, Peter Ohler, All rights reserved.

package slip

import (
	"io"
)

// IOStreamSymbol is the symbol with a value of "io-stream".
const IOStreamSymbol = Symbol("io-stream")

func init() {
	DefConstant(IOStreamSymbol, IOStreamSymbol, `A _io-stream_ stream backed by a io.ReadWriter.`)
}

// IOStream is a io.ReadWriter.
type IOStream struct {
	RW io.ReadWriter
}

// String representation of the Object.
func (obj *IOStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *IOStream) Append(b []byte) []byte {
	return append(b, "#<IO-STREAM>"...)
}

// Simplify the Object into an int64.
func (obj *IOStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *IOStream) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *IOStream) Hierarchy() []Symbol {
	return []Symbol{IOStreamSymbol, StreamSymbol, TrueSymbol}
}

// StreamType returns 'ioStream.
func (obj *IOStream) StreamType() Symbol {
	return IOStreamSymbol
}

// Eval returns self.
func (obj *IOStream) Eval(s *Scope, depth int) Object {
	return obj
}

// Write made visible since io.Write functions are not automatically visible.
func (obj *IOStream) Write(b []byte) (int, error) {
	return obj.RW.Write(b)
}

// Read made visible since io.Read functions are not automatically visible.
func (obj *IOStream) Read(b []byte) (cnt int, err error) {
	return obj.RW.Read(b)
}

// Close made visible since os.File functions are not automatically visible.
func (obj *IOStream) Close() (err error) {
	if closer, ok := obj.RW.(io.Closer); ok {
		err = closer.Close()
	}
	return
}

// IsOpen return true if the stream is open or false if not.
func (obj *IOStream) IsOpen() bool {
	_, err := obj.RW.Write([]byte{})
	return err == nil
}
