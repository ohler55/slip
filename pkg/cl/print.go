// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Print{Function: slip.Function{Name: "print", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "print",
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
			Text: `__print__ writes a string representation of the _object_ to the provided _output-stream_.
Output is produced as if _*print-escape*_ is _false_. A newline is prepended to the output and a space is
appended to the output.
If the _output-stream_ is not provided then the _*standard-output*_ is used. The _object_ is returned.`,
			Examples: []string{
				"(print 123) => 123 ;; \n123 is written",
			},
		}, &slip.CLPkg)
}

// Print represents the print function.
type Print struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Print) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	p := *slip.DefaultPrinter()
	p.Escape = true
	obj := args[0]
	var w io.Writer = slip.StandardOutput.(io.Writer)
	if 1 < len(args) {
		var ok bool
		if w, ok = args[1].(io.Writer); !ok {
			slip.PanicType("print output-stream", args[1], "output-stream")
		}
	}
	b := p.Append([]byte{'\n'}, obj, 0)
	b = append(b, ' ')
	if _, err := w.Write(b); err != nil {
		panic(err)
	}
	return obj
}
