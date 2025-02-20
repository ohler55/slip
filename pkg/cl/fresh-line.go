// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FreshLine{Function: slip.Function{Name: "fresh-line", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fresh-line",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text: `__fresh-line__ writes a string representation of the _object_ to the provided _output-stream_.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(fresh-line) => nil ;; a newline is written",
			},
		}, &slip.CLPkg)
}

// FreshLine represents the fresh-line function.
type FreshLine struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FreshLine) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 0, 1)
	var (
		w     io.Writer = slip.StandardOutput.(io.Writer)
		ss, _           = slip.StandardOutput.(slip.Stream)
	)
	if 0 < len(args) {
		var ok bool
		ss, _ = args[0].(slip.Stream)
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType("fresh-line output-stream", args[0], "output-stream")
		}
	}
	if peeker, ok := ss.(slip.LastBytePeeker); !ok || peeker.LastByte() != '\n' {
		if _, err := w.Write([]byte{'\n'}); err != nil {
			slip.PanicStream(ss, "fresh-line write failed. %s", err)
		}
		result = slip.True
	}
	return
}
