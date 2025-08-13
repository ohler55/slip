// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"
	"os"

	"github.com/ohler55/slip"
)

// DribblerSymbol is the symbol with a value of "dribbler".
const DribblerSymbol = slip.Symbol("dribbler")

// Dribbler is a stream that writes to a dribble file on Read and Writer.
type Dribbler struct {
	origIn  slip.Object
	origOut slip.Object
	input   io.Reader
	output  io.Writer
	file    *os.File
}

// String representation of the Object.
func (obj *Dribbler) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *Dribbler) Append(b []byte) []byte {
	return append(b, "#<DRIBBLER>"...)
}

// Simplify the Object into a string.
func (obj *Dribbler) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *Dribbler) Equal(other slip.Object) bool {
	return obj == other
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *Dribbler) Hierarchy() []slip.Symbol {
	return []slip.Symbol{DribblerSymbol, slip.StreamSymbol, slip.TrueSymbol}
}

// StreamType returns 'dribbler.
func (obj *Dribbler) StreamType() slip.Symbol {
	return DribblerSymbol
}

// Eval returns self.
func (obj *Dribbler) Eval(s *slip.Scope, depth int) slip.Object {
	return obj
}

// IsOpen returns true.
func (obj *Dribbler) IsOpen() bool {
	return true
}

// Read lets the original read and then dribbles what was read to the file
// with a "< " prefix.
func (d *Dribbler) Read(b []byte) (n int, err error) {
	if n, err = d.input.Read(b); err == nil {
		_, err = fmt.Fprintf(d.file, "< %s\n", b[:n])
	}
	return
}

// Write to the original and then dribbles the same output to the file with a
// "> " prefix.
func (d *Dribbler) Write(b []byte) (n int, err error) {
	if n, err = d.output.Write(b); err == nil {
		_, err = fmt.Fprintf(d.file, "> %s\n", b[:n])
	}
	return
}

// LoadForm returns a form that can be evaluated to create the object.
func (d *Dribbler) LoadForm() slip.Object {
	return d
}
