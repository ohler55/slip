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
			Text: `__terpri__ writes a string representation of the _object_ to the provided _output-stream_.
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

// Call the function with the arguments provided.
func (f *Terpri) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 0, 1)
	w := slip.StandardOutput.(io.Writer)
	ss, _ := slip.StandardOutput.(slip.Stream)
	if 0 < len(args) {
		var ok bool
		ss, _ = args[0].(slip.Stream)
		if w, ok = args[0].(io.Writer); !ok {
			slip.TypePanic(s, depth, "terpri output-stream", args[0], "output-stream")
		}
	}
	if _, err := w.Write([]byte{'\n'}); err != nil {
		slip.StreamPanic(s, depth, ss, "terpri write failed. %s", err)
	}
	return nil
}
