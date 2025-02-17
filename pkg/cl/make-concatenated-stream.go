// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeConcatenatedStream{Function: slip.Function{Name: "make-concatenated-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-concatenated-stream",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "streams",
					Type: "input-stream",
					Text: "Streams to read from.",
				},
			},
			Return: "concatenated-stream",
			Text:   `__make-concatenated-stream__ returns a _concatenated-stream_ with the component _streams_.`,
			Examples: []string{
				`(make-concatenated-stream (make-string-input-stream)) => #<CONCATENATED-STREAM>`,
			},
		}, &slip.CLPkg)
}

// MakeConcatenatedStream represents the make-concatenated-stream function.
type MakeConcatenatedStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeConcatenatedStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return NewConcatenatedStream(args...)
}
