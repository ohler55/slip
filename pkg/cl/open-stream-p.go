// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := OpenStreamP{Function: slip.Function{Name: "open-stream-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "open-stream-p",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "stream",
					Text: "The stream to check if open.",
				},
			},
			Return: "boolean",
			Text:   `__open-stream-p__ returns _true_ if _stream_ is an open stream.`,
			Examples: []string{
				`(setq out (make-string-output-stream)`,
				"(open-stream-p out) => t",
			},
		}, &slip.CLPkg)
}

// OpenStreamP represents the open-stream-p function.
type OpenStreamP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *OpenStreamP) Call(s *slip.Scope, args slip.List, depth int) (open slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	stream, ok := args[0].(slip.Stream)
	if !ok {
		slip.PanicType("stream", args[0], "stream")
	}
	if stream.IsOpen() {
		open = slip.True
	}
	return
}
