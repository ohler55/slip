// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := GetOutputStreamString{Function: slip.Function{Name: "get-output-stream-string", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "get-output-stream-string",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "string-output-stream",
					Text: "The string-stream to get a string from.",
				},
			},
			Return: "string",
			Text: `__get-output-stream-string__ returns the accumulated string from the _stream_ and clears
the _stream_ backing string.`,
			Examples: []string{
				`(let ((sos (make-string-output-stream)))`,
				`(princ "abc")`,
				` (get-output-stream-string sos)) => "abc"`,
			},
		}, &slip.CLPkg)
}

// GetOutputStreamString represents the get-output-stream-string function.
type GetOutputStreamString struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *GetOutputStreamString) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	os, ok := args[0].(*slip.StringStream)
	if !ok {
		slip.PanicType("stream", args[0], "string-output-stream")
	}
	return slip.String(os.Content())
}
