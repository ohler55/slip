// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := OutputStreamp{Function: slip.Function{Name: "output-stream-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "output-stream-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__output-stream-p__ returns _true_ if _object_ is an output stream.`,
			Examples: []string{
				`(setq out (make-string-output-stream)`,
				"(output-stream-p out) => t",
				"(output-stream-p t) => nil",
			},
		}, &slip.CLPkg)
}

// OutputStreamp represents the output-stream-p function.
type OutputStreamp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *OutputStreamp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(io.Writer); ok {
		return slip.True
	}
	return nil
}
