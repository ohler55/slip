// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

// TwoWayStreamSymbol is the symbol with a value of "two-way-stream".
const TwoWayStreamSymbol = slip.Symbol("two-way-stream")

func init() {
	slip.DefConstant(TwoWayStreamSymbol, TwoWayStreamSymbol,
		`An _two-way-stream_ accepts input and writes output stream.`)
}

// TwoWayStream is a stream that accepts input from an input stream and writes
// to an output stream.
type TwoWayStream struct {
	input  io.Reader
	output io.Writer
}

// NewTwoWayStream creates a new TwoWayStream.
func NewTwoWayStream(input io.Reader, output io.Writer) *TwoWayStream {
	return &TwoWayStream{input: input, output: output}
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
	if es, ok := other.(*TwoWayStream); ok && es.input == obj.input && es.output == obj.output {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *TwoWayStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{TwoWayStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'echoStream.
func (obj *TwoWayStream) StreamType() slip.Symbol {
	return TwoWayStreamSymbol
}

// Eval returns self.
func (obj *TwoWayStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Close the stream but not the input or output streams.
func (obj *TwoWayStream) Close() error {
	obj.input = nil
	obj.output = nil

	return nil
}

// IsOpen return true if the stream is open or false if not.
func (obj *TwoWayStream) IsOpen() bool {
	return obj.input != nil && obj.output != nil
}

// Read from the current position in the buf. This is part of the io.Reader interface.
func (obj *TwoWayStream) Read(p []byte) (n int, err error) {
	if obj.input == nil {
		slip.PanicStream(obj, "closed")
	}
	return obj.input.Read(p)
}

// Write to all the component streams. If any one of the writes fails a panic
// is called.
func (obj *TwoWayStream) Write(b []byte) (n int, err error) {
	if obj.output == nil {
		slip.PanicStream(obj, "closed")
	}
	return obj.output.Write(b)
}
