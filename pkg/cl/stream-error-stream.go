// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StreamErrorStream{Function: slip.Function{Name: "stream-error-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "stream-error-stream",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "stream-error",
					Text: "The stream-error or subclass of stream-error to get the stream of.",
				},
			},
			Return: "object",
			Text: `__stream-error-stream__ returns the value of the _stream_ slot in the _condition_
which must be of stream _stream-error_ or inherit from _stream-error_.`,
			Examples: []string{
				`(stream-error-stream (make-condition 'stream-error :stream *standard-output*)) => #<>)`,
			},
		}, &slip.CLPkg)
}

// StreamErrorStream represents the stream-error-stream function.
type StreamErrorStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StreamErrorStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	cond, ok := args[0].(slip.StreamError)
	if !ok {
		slip.PanicUnboundSlot(args[0], slip.Symbol("stream"), "")
	}
	return cond.Stream()
}
