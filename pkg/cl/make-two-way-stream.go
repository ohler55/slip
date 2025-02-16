// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeTwoWayStream{Function: slip.Function{Name: "make-two-way-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-two-way-stream",
			Args: []*slip.DocArg{
				{
					Name: "input-stream",
					Type: "input-stream",
					Text: "Stream to read from.",
				},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "Stream to write to.",
				},
			},
			Return: "two-way-stream",
			Text:   `__make-two-way-stream__ returns an _two-way-stream_ with the _input-stream_ and _output-stream_.`,
			Examples: []string{
				`(let* ((ss1 (make-string-input-stream "abc"))`,
				`       (ss2 (make-string-output-stream)))`,
				` (make-two-way-stream ss1 ss2)) => #<TWO-WAY-STREAM>`,
			},
		}, &slip.CLPkg)
}

// MakeTwoWayStream represents the make-two-way-stream function.
type MakeTwoWayStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeTwoWayStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	tws := TwoWayStream{}
	var ok bool
	if tws.input, ok = args[0].(io.Reader); !ok {
		slip.PanicType("input-stream", args[0], "input-stream")
	}
	if tws.output, ok = args[1].(io.Writer); !ok {
		slip.PanicType("output-stream", args[1], "output-stream")
	}
	return &tws
}
