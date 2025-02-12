// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeBroadcastStream{Function: slip.Function{Name: "make-broadcast-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-broadcast-stream",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "streams",
					Type: "output-stream",
					Text: "Streams to broadcast to.",
				},
			},
			Return: "broadcvast-stream",
			Text:   `__make-broadcast-stream__ returns a _broadcast-stream_ with the component _streams_.`,
			Examples: []string{
				`(make-broadcast-stream "this is the input") => #<INPUT-STREAM>`,
			},
		}, &slip.CLPkg)
}

// MakeBroadcastStream represents the make-broadcast-stream function.
type MakeBroadcastStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeBroadcastStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	bs := make(BroadcastStream, len(args))
	for i, a := range args {
		if stream, ok := a.(slip.Stream); ok {
			if _, ok = stream.(io.Writer); ok {
				bs[i] = stream
				continue
			}
		}
		slip.PanicType("output-stream", a, "stream")
	}
	return bs
}
