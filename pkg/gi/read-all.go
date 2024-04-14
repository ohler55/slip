// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadAll{Function: slip.Function{Name: "read-all", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-all",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "input-stream",
					Text: "The stream to read from.",
				},
			},
			Return: "string",
			Text:   `__read-all__ reads all characters from _stream_.`,
			Examples: []string{
				`(read-all (make-string-input-stream "abc")) => "abc"`,
			},
		}, &Pkg)
}

// ReadAll represents the read-all function.
type ReadAll struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadAll) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	r, ok := args[0].(io.Reader)
	if !ok {
		slip.PanicType("stream", args[0], "input-stream")
	}
	b, err := io.ReadAll(r)
	if err != nil {
		stream, _ := args[0].(slip.Stream)
		slip.PanicStream(stream, "read failed: %s", err)
	}
	return slip.String(b)
}
