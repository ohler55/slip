// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

// EchoStreamSymbol is the symbol with a value of "echo-stream".
const EchoStreamSymbol = slip.Symbol("echo-stream")

func init() {
	slip.CLPkg.Locked = false // a bit of a cheat
	slip.CLPkg.DefConst(string(EchoStreamSymbol), EchoStreamSymbol,
		`An _echo-stream_ accepts input from an input stream and echos that input to an output stream.`)
	slip.CLPkg.Locked = true
	slip.CLPkg.Export(string(EchoStreamSymbol))
}

// EchoStream is a stream that accepts input from an input stream and echos
// that input to an output stream.
type EchoStream struct {
	input  io.Reader
	output io.Writer
}

// NewEchoStream creates a new EchoStream.
func NewEchoStream(input io.Reader, output io.Writer) *EchoStream {
	return &EchoStream{input: input, output: output}
}

// String representation of the Object.
func (obj *EchoStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *EchoStream) Append(b []byte) []byte {
	return append(b, "#<ECHO-STREAM>"...)
}

// Simplify the Object into a string.
func (obj *EchoStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *EchoStream) Equal(other slip.Object) (eq bool) {
	if es, ok := other.(*EchoStream); ok && es.input == obj.input && es.output == obj.output {
		eq = true
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *EchoStream) Hierarchy() []slip.Symbol {
	return []slip.Symbol{EchoStreamSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'echoStream.
func (obj *EchoStream) StreamType() slip.Symbol {
	return EchoStreamSymbol
}

// Eval returns self.
func (obj *EchoStream) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// Close the stream but not the input or output streams.
func (obj *EchoStream) Close() error {
	obj.input = nil
	obj.output = nil

	return nil
}

// IsOpen returns true if the stream is open or false if not.
func (obj *EchoStream) IsOpen() bool {
	return obj.input != nil && obj.output != nil
}

// Read from the current position in the buf. This is part of the io.Reader interface.
func (obj *EchoStream) Read(p []byte) (n int, err error) {
	if obj.input == nil {
		slip.StreamPanic(slip.NewScope(), 0, obj, "closed")
	}
	if n, err = obj.input.Read(p); err == nil {
		_, err = obj.output.Write(p[:n])
	}
	return
}

// Write to all the component streams. If any one of the writes fails a panic
// is called.
func (obj *EchoStream) Write(b []byte) (n int, err error) {
	if obj.output == nil {
		slip.StreamPanic(slip.NewScope(), 0, obj, "closed")
	}
	return obj.output.Write(b)
}
