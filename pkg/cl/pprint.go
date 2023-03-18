// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Pprint{Function: slip.Function{Name: "pprint", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "pprint",
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
			Return: "",
			Text: `__pprint__ writes a string representation of the _object_ to the provided _output-stream_.
Output is produced as if _*pprint-escape*_ is _false_ and _*print-pretty*_ is non-nil. A newline is
prepended to the output.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(pprint 123) => 123 ;; \n123 is written",
			},
		}, &slip.CLPkg)
}

// Pprint represents the pprint function.
type Pprint struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Pprint) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	p := *slip.DefaultPrinter()
	p.Escape = true
	p.Pretty = true
	obj := args[0]
	var w io.Writer = slip.StandardOutput.(io.Writer)
	if 1 < len(args) {
		var ok bool
		if w, ok = args[1].(io.Writer); !ok {
			slip.PanicType("pprint output-stream", args[1], "output-stream")
		}
	}
	b := p.Append([]byte{'\n'}, obj, 0)
	if _, err := w.Write(b); err != nil {
		panic(err)
	}
	return slip.Novalue
}
