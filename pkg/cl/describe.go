// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

const (
	bold     = "\x1b[1m"
	colorOff = "\x1b[m"
	spaces   = "                                                                                "
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Describe{Function: slip.Function{Name: "describe", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "describe",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to be described.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Text: `The __describe__ function writes a description of the _object_ to the provided _output-stream_.
If the _output-stream_ is not provided then the _*standard-output*_ is used.`,
			Examples: []string{
				"(describe 123) => nil ;; 123 [fixnum] is written",
			},
		}, &slip.CLPkg)
}

// Describe represents the describe function.
type Describe struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Describe) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	obj := args[len(args)-1]
	var w io.Writer = slip.StandardOutput.(io.Writer)
	if 1 < len(args) {
		var ok bool
		if w, ok = args[0].(io.Writer); !ok {
			slip.PanicType("describe output-stream", args[0], "output-stream")
		}
	}
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	var b []byte
	if ansi {
		b = append(b, bold...)
		b = slip.Append(b, obj)
		b = append(b, colorOff...)
	} else {
		b = slip.Append(b, obj)
	}
	b = append(b, "\n  ["...)
	if obj == nil {
		b = append(b, "null"...)
	} else {
		b = append(b, string(obj.Hierarchy()[0])...)
	}
	b = append(b, "]\n"...)
	indent := 0
Details:
	switch to := obj.(type) {
	case slip.Symbol:
		b = append(b, '\n')
		if s.Has(to) {
			obj = s.Get(to)
		} else if fi := slip.CurrentPackage.Funcs[string(to)]; fi != nil {
			obj = fi
		}
		if obj != to {
			if ansi {
				b = append(b, bold...)
				b = slip.Append(b, to)
				b = append(b, colorOff...)
			} else {
				b = slip.Append(b, to)
			}
			b = append(b, " names a "...)
			if obj == nil {
				b = append(b, "null"...)
			} else {
				b = append(b, string(obj.Hierarchy()[0])...)
			}
			b = append(b, ":\n"...)
			indent += 2
			goto Details
		}
	case slip.Describer:
		b = to.Describe(b, indent, right, ansi)
	default:
		b = append(b, spaces[:indent]...)
		b = append(b, "Value = "...)
		b = slip.Append(b, to)
		b = append(b, '\n')
	}
	if _, err := w.Write(b); err != nil {
		panic(err)
	}
	return slip.Novalue
}
