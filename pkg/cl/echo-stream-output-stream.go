// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := EchoStreamOutputStream{Function: slip.Function{Name: "echo-stream-output-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "echo-stream-output-stream",
			Args: []*slip.DocArg{
				{
					Name: "echo-stream",
					Type: "echo-stream",
					Text: "The echo-stream to return the output stream of.",
				},
			},
			Return: "output-stream",
			Text:   `__echo-stream-output-stream__ returns the output stream of _echo-stream_`,
			Examples: []string{
				`(let* ((ss1 (make-string-output-stream "abc"))`,
				`       (ss2 (make-string-output-stream))`,
				`       (es (make-echo-stream ss1 ss2)))`,
				` (echo-stream-output-stream es)) => #<string-stream>`,
			},
		}, &slip.CLPkg)
}

// EchoStreamOutputStream represents the echo-stream-output-stream function.
type EchoStreamOutputStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *EchoStreamOutputStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	es, ok := args[0].(*EchoStream)
	if !ok {
		slip.TypePanic(s, depth, "echo-stream", args[0], "echo-stream")
	}
	return es.output.(slip.Object)
}
