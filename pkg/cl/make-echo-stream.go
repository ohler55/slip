// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeEchoStream{Function: slip.Function{Name: "make-echo-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-echo-stream",
			Args: []*slip.DocArg{
				{
					Name: "input-stream",
					Type: "input-stream",
					Text: "Stream to gather input from.",
				},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "Stream to echo input to.",
				},
			},
			Return: "echo-stream",
			Text:   `__make-echo-stream__ returns an _echo-stream_ with the _input-stream_ and _output-stream_.`,
			Examples: []string{
				`(let* ((ss1 (make-string-input-stream "abc"))`,
				`       (ss2 (make-string-output-stream)))`,
				` (make-echo-stream ss1 ss2)) => #<ECHO-STREAM>`,
			},
		}, &slip.CLPkg)
}

// MakeEchoStream represents the make-echo-stream function.
type MakeEchoStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeEchoStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	es := EchoStream{}
	var ok bool
	if es.input, ok = args[0].(io.Reader); !ok {
		slip.PanicType("input-stream", args[0], "input-stream")
	}
	if es.output, ok = args[1].(io.Writer); !ok {
		slip.PanicType("output-stream", args[1], "output-stream")
	}
	return &es
}
