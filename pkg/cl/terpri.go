// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Terpri{Function: slip.Function{Name: "terpri", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "terpri",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text: `writes a string representation of the _object_ to the provided _output-stream_.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(terpri) => nil ;; a newline is written",
			},
		}, &slip.CLPkg)
}

// Terpri represents the terpri function.
type Terpri struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Terpri) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if 1 < len(args) {
		slip.PanicArgCount(f, 0, 1)
	}
	var w io.Writer = slip.StandardOutput.(io.Writer)
	if 0 < len(args) {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType("terpri output-stream", args[0], "output-stream")
		}
	}
	if _, err := w.Write([]byte{'\n'}); err != nil {
		panic(err)
	}
	return nil
}
