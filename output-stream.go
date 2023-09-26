// Copyright (c) 2022, Peter Ohler, All rights reserved.

package slip

import (
	"io"
)

// OutputStreamSymbol is the symbol with a value of "output-stream".
const OutputStreamSymbol = Symbol("output-stream")

func init() {
	DefConstant(OutputStreamSymbol, OutputStreamSymbol, `A _output-stream_ stream backed by a io.Writer.`)
}

// OutputStream is a *os.Output.
type OutputStream struct {
	Writer io.Writer
}

// String representation of the Object.
func (obj *OutputStream) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *OutputStream) Append(b []byte) []byte {
	return append(b, "#<OUTPUT-STREAM>"...)
}

// Simplify the Object into an int64.
func (obj *OutputStream) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *OutputStream) Equal(other Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *OutputStream) Hierarchy() []Symbol {
	return []Symbol{OutputStreamSymbol, StreamSymbol, TrueSymbol}
}

// StreamType returns 'outputStream.
func (obj *OutputStream) StreamType() Symbol {
	return OutputStreamSymbol
}

// Eval returns self.
func (obj *OutputStream) Eval(s *Scope, depth int) Object {
	return obj
}

// Write made visible since os.Output functions are not automatically visible.
func (obj *OutputStream) Write(b []byte) (int, error) {
	return obj.Writer.Write(b)
}
