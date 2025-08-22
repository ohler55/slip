// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TwoWayStreamOutputStream{Function: slip.Function{Name: "two-way-stream-output-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "two-way-stream-output-stream",
			Args: []*slip.DocArg{
				{
					Name: "two-way-stream",
					Type: "two-way-stream",
					Text: "The two-way-stream to return the output stream of.",
				},
			},
			Return: "output-stream",
			Text:   `__two-way-stream-output-stream__ returns the output stream of _two-way-stream_`,
			Examples: []string{
				`(let* ((ss1 (make-string-output-stream "abc"))`,
				`       (ss2 (make-string-output-stream))`,
				`       (es (make-two-way-stream ss1 ss2)))`,
				` (two-way-stream-output-stream es)) => #<string-stream>`,
			},
		}, &slip.CLPkg)
}

// TwoWayStreamOutputStream represents the two-way-stream-output-stream function.
type TwoWayStreamOutputStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *TwoWayStreamOutputStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	es, ok := args[0].(*TwoWayStream)
	if !ok {
		slip.TypePanic(s, depth, "two-way-stream", args[0], "two-way-stream")
	}
	return es.Output.(slip.Object)
}
