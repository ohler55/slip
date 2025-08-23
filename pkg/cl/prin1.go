// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Prin1{Function: slip.Function{Name: "prin1", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "prin1",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The _object_ to be printed.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "object",
			Text: `__prin1__ writes a string representation of the _object_ to the provided _output-stream_.
Output is produced as if _*print-escape*_ is _true_ and _*print-readably*_ is _true_.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(prin1 123) => 123 ;; 123 is written",
			},
		}, &slip.CLPkg)
}

// Prin1 represents the prin1 function.
type Prin1 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Prin1) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)
	p.Escape = true
	p.Readably = true
	obj := args[0]
	w := slip.StandardOutput.(io.Writer)
	ss, _ := slip.StandardOutput.(slip.Stream)
	if 1 < len(args) {
		var ok bool
		ss, _ = args[1].(slip.Stream)
		if w, ok = args[1].(io.Writer); !ok {
			slip.TypePanic(s, depth, "prin1 output-stream", args[1], "output-stream")
		}
	}
	if _, err := w.Write(p.Append([]byte{}, obj, 0)); err != nil {
		slip.StreamPanic(s, depth, ss, "prin1 write failed. %s", err)
	}
	return obj
}
