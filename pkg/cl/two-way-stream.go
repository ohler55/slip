// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

// TwoWayStreamSymbol is the symbol with a value of "two-way-stream".
const TwoWayStreamSymbol = slip.Symbol("two-way-stream")

func init() {
	slip.CLPkg.Locked = false // a bit of a cheat
	slip.CLPkg.DefConst(string(TwoWayStreamSymbol), TwoWayStreamSymbol,
		`An _two-way-stream_ accepts input and writes output stream.`)
	slip.CLPkg.Locked = true
	slip.CLPkg.Export(string(TwoWayStreamSymbol))
}

// TwoWayStream is a stream that accepts input from an input stream and writes
// to an output stream.
type TwoWayStream struct {
	// Input io.Reader
	slip.RuneReader
	Output io.Writer
}

// NewTwoWayStream creates a new TwoWayStream.
func NewTwoWayStream(input io.Reader, output io.Writer) *TwoWayStream {
	return &TwoWayStream{RuneReader: slip.RuneReader{Reader: input}, Output: output}
}

// String representation of the Object.
func (obj *TwoWayStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *TwoWayStream) Append(b []byte) []byte {
	return append(b, "#<TWO-WAY-STREAM>"...)
}

// Simplify the Object into a string.
func (obj *TwoWayStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *TwoWayStream) Equal(other slip.Object) (eq bool) {
	if es, ok := other.(*TwoWayStream); ok && es.Reader == obj.Reader && es.Output == obj.Output {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *TwoWayStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{TwoWayStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'two-way-stream.
func (obj *TwoWayStream) StreamType() slip.Symbol {
	return TwoWayStreamSymbol
}

// Eval returns self.
func (obj *TwoWayStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Close the stream but not the input or output streams.
func (obj *TwoWayStream) Close() error {
	obj.Reader = nil
	obj.Output = nil

	return nil
}

// IsOpen return true if the stream is open or false if not.
func (obj *TwoWayStream) IsOpen() bool {
	return obj.Reader != nil && obj.Output != nil
}

// Write to all the component streams. If any one of the writes fails a panic
// is called.
func (obj *TwoWayStream) Write(b []byte) (n int, err error) {
	if obj.Output == nil {
		slip.StreamPanic(slip.NewScope(), 0, obj, "closed")
	}
	return obj.Output.Write(b)
}
