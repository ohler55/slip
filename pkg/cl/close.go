// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Close{Function: slip.Function{Name: "close", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "close",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The filepath to close.",
				},
				{Name: "&key"},
				{
					Name: "abort",
					Type: "boolean",
					Text: "Currently ignored.",
				},
			},
			Return: "file-stream",
			Text:   `__close__ closes _stream_.`,
			Examples: []string{
				`(close "../*.lisp") => ("/top/one/x.lisp" "/top/one/y.lisp")`,
			},
		}, &slip.CLPkg)
}

// Close represents the close function.
type Close struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Close) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	closer, ok := args[0].(io.Closer)
	if !ok {
		slip.PanicType("stream", args[0], "stream")
	}
	closer.Close()

	return slip.True
}
