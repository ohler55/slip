// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import "fmt"

// EndOfFileSymbol is the symbol with a value of "end-of-file".
const EndOfFileSymbol = Symbol("end-of-file")

// EndOfFileNew returns a new EndOfFile (end-of-file) describing a stream
// error.
func EndOfFileNew(s *Scope, depth int, stream Stream, format string, args ...any) Object {
	c := FindClass("end-of-file")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":stream"), stream,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// EndOfFilePanic raises a EndOfFile (end-of-file) describing a stream
// error.
func EndOfFilePanic(s *Scope, depth int, stream Stream, format string, args ...any) {
	panic(EndOfFileNew(s, depth, stream, format, args...))
}
