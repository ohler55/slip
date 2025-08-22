// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PrintObject{Function: slip.Function{Name: "print-object", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "print-object",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: `The object to write to stream.`,
				},
				{
					Name: "stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text:   `__print-object__ write to the _stream_.`,
			Examples: []string{
				"(print-object 'cymbal *standard-output*) => nil",
				" ;; prints cymbal",
			},
		}, &slip.CLPkg)
}

// PrintObject represents the print-object function.
type PrintObject struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PrintObject) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	w, ok := args[1].(io.Writer)
	if !ok {
		slip.TypePanic(s, depth, "stream", args[1], "output-stream")
	}
	if _, err := w.Write(slip.Append(nil, args[0])); err != nil {
		panic(err)
	}
	return nil
}
