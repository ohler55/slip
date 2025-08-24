// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := StreamElementType{Function: slip.Function{Name: "stream-element-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "stream-element-type",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "stream",
					Text: "The stream to return the element type of.",
				},
			},
			Return: "symbol",
			Text:   `__stream-element-type__ returns the element type of _stream_.`,
			Examples: []string{
				`(setq out (make-string-output-stream)`,
				"(stream-element-type out) => octet",
			},
		}, &slip.CLPkg)
}

// StreamElementType represents the stream-element-type function.
type StreamElementType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *StreamElementType) Call(s *slip.Scope, args slip.List, depth int) (open slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(slip.Stream); !ok {
		slip.TypePanic(s, depth, "stream", args[0], "stream")
	}
	return slip.OctetSymbol
}
