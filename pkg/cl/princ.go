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

// Call the function with the arguments provided.
func (f *Princ) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	p := *slip.DefaultPrinter()
	p.ScopedUpdate(s)
	p.Escape = false
	p.Readably = false
	obj := args[0]
	w := slip.StandardOutput.(io.Writer)
	ss, _ := slip.StandardOutput.(slip.Stream)
	if 1 < len(args) {
		var ok bool
		ss, _ = args[1].(slip.Stream)
		if w, ok = args[1].(io.Writer); !ok {
			slip.TypePanic(s, depth, "princ output-stream", args[1], "output-stream")
		}
	}
	var err error
	if ss, _ := obj.(slip.String); 0 < len(ss) {
		_, err = w.Write([]byte(ss))
	} else {
		var b []byte
		if sa, ok := obj.(slip.ScopedAppender); ok {
			b = sa.ScopedAppend(b, s, &p, 0)
		} else {
			b = p.Append(b, obj, 0)
		}
		_, err = w.Write(b)
	}
	if err != nil {
		slip.StreamPanic(s, depth, ss, "princ write failed. %s", err)
	}
	return obj
}
