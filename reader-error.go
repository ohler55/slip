// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ReaderErrorSymbol is the symbol with a value of "reader-error".
const ReaderErrorSymbol = Symbol("reader-error")

// NewReaderError creates a ReaderPanic (reader-error) describing a parse error.
func NewReaderError(stream Stream, format string, args ...any) Object {
	c := FindClass("reader-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":stream"), stream,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicReader raises a ReaderPanic (reader-error) describing a parse
// error.
func PanicReader(stream Stream, format string, args ...any) {
	panic(NewReaderError(stream, format, args...))
}
