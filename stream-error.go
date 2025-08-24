// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// StreamErrorSymbol is the symbol with a value of "stream-error".
const StreamErrorSymbol = Symbol("stream-error")

// StreamErrorNew returns a new StreamError (stream-error) describing a stream
// error.
func StreamErrorNew(s *Scope, depth int, stream Stream, format string, args ...any) Object {
	c := FindClass("stream-error")
	obj := c.MakeInstance()

	obj.Init(s, List{
		Symbol(":stream"), stream,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, depth)
	return obj
}

// StreamPanic raises a StreamError (stream-error) describing a stream error.
func StreamPanic(s *Scope, depth int, stream Stream, format string, args ...any) {
	panic(StreamErrorNew(s, depth, stream, format, args...))
}
