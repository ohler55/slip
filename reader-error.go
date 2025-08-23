// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// ReaderErrorSymbol is the symbol with a value of "reader-error".
const ReaderErrorSymbol = Symbol("reader-error")

// ReaderErrorNew creates a ReaderError (reader-error) describing a parse
// error.
func ReaderErrorNew(s *Scope, depth int, stream Stream, format string, args ...any) Object {
	c := FindClass("reader-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":stream"), stream,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// ReaderPanic raises a ReaderError (reader-error) describing a parse
// error.
func ReaderPanic(s *Scope, depth int, stream Stream, format string, args ...any) {
	panic(ReaderErrorNew(s, depth, stream, format, args...))
}
