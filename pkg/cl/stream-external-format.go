// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StreamExternalFormat{Function: slip.Function{Name: "stream-external-format", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "stream-external-format",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "stream",
					Text: "The stream to return the external format of.",
				},
			},
			Return: "symbol",
			Text:   `__stream-external-format__ returns the external format of _stream_.`,
			Examples: []string{
				`(setq out (make-string-output-stream)`,
				"(stream-external-format out) => :default",
			},
		}, &slip.CLPkg)
}

// StreamExternalFormat represents the stream-external-format function.
type StreamExternalFormat struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StreamExternalFormat) Call(s *slip.Scope, args slip.List, depth int) (open slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.Stream); !ok {
		slip.TypePanic(s, depth, "stream", args[0], "stream")
	}
	return slip.Symbol(":default")
}
