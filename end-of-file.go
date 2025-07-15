// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// EndOfFileSymbol is the symbol with a value of "end-of-file".
const EndOfFileSymbol = Symbol("end-of-file")

// NewEndOfFile returns a new EndOfFilePanic (end-of-file) describing a stream
// error.
func NewEndOfFile(stream Stream, format string, args ...any) Object {
	c := FindClass("end-of-file")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":stream"), stream,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicStream raises a EndOfFilePanic (end-of-file) describing a stream
// error.
func PanicEndOfFile(stream Stream, format string, args ...any) {
	panic(NewEndOfFile(stream, format, args...))
}
