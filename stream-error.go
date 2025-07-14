// Copyright (c) 2023, Peter Ohler, All rights reserved.

package slip

import "fmt"

// StreamErrorSymbol is the symbol with a value of "stream-error".
const StreamErrorSymbol = Symbol("stream-error")

// NewStreamError returns a new StreamPanic (stream-error) describing a stream
// error.
func NewStreamError(stream Stream, format string, args ...any) Object {
	c := FindClass("stream-error")
	obj := c.MakeInstance()

	obj.Init(NewScope(), List{
		Symbol(":stream"), stream,
		Symbol(":message"), String(fmt.Sprintf(format, args...)),
	}, 0)
	return obj
}

// PanicStream raises a StreamPanic (stream-error) describing a stream
// error.
func PanicStream(stream Stream, format string, args ...any) {
	panic(NewStreamError(stream, format, args...))
}
