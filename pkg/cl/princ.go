// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Princ{Function: slip.Function{Name: "princ", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "princ",
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
			Text: `__princ__ writes a string representation of the _object_ to the provided _output-stream_.
Output is produced as if _*print-escape*_ is _false_ and _*print-readably*_ is _false_.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(princ 123) => 123 ;; 123 is written",
			},
		}, &slip.CLPkg)
}

// Princ represents the princ function.
type Princ struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Princ) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	p := *slip.DefaultPrinter()
	p.Escape = false
	p.Readably = false
	obj := args[0]
	var w io.Writer = slip.StandardOutput.(io.Writer)
	if 1 < len(args) {
		var ok bool
		if w, ok = args[1].(io.Writer); !ok {
			slip.PanicType("princ output-stream", args[1], "output-stream")
		}
	}
	var err error
	if s, _ := obj.(slip.String); 0 < len(s) {
		_, err = w.Write([]byte(s))
	} else {
		_, err = w.Write(p.Append([]byte{}, obj, 0))
	}
	if err != nil {
		panic(err)
	}
	return obj
}
