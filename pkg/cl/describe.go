// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"io"

	"github.com/ohler55/slip"
)

const (
	bold      = "\x1b[1m"
	underline = "\x1b[4m"
	colorOff  = "\x1b[m"
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
					Text: "The _object_ to be described.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Text: `writes a description of the _object_ to the provided _output-stream_.
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
	switch to := obj.(type) {
	case slip.Symbol:
		fmt.Printf("*** %s names ??\n", to)

	case *slip.FuncInfo:
		b = append(b, '\n')
		b = f.appendFunc(b, to, "", ansi)
	}
	if _, err := w.Write(b); err != nil {
		panic(err)
	}
	return slip.Novalue
}

func (f *Describe) appendFunc(b []byte, fi *slip.FuncInfo, indent string, ansi bool) []byte {
	b = append(b, indent...)
	b = append(b, "Lambda-List: ("...)
	if ansi {
		b = append(b, bold...)
		b = slip.Append(b, slip.Symbol(fi.Name))
		b = append(b, colorOff...)
	} else {
		b = slip.Append(b, slip.Symbol(fi.Name))
	}
	b = append(b, " ("...)
	for i, da := range fi.Doc.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		if da.Default == nil {
			b = append(b, da.Name...)
		} else {
			b = append(b, '(')
			b = append(b, da.Name...)
			b = append(b, ' ')
			b = slip.Append(b, da.Default)
			b = append(b, ')')
		}
	}
	b = append(b, ") ...)\n"...)

	if 0 < len(fi.Doc.Return) {
		b = append(b, indent...)
		b = append(b, "Return: "...)
		b = append(b, fi.Doc.Return...)
		b = append(b, '\n')
	}
	i2 := make([]byte, 0, len(indent)+2)
	i2 = append(i2, indent...)
	i2 = append(i2, "  "...)
	i3 := make([]byte, 0, len(i2)+2)
	i3 = append(i3, i2...)
	i3 = append(i3, "  "...)
	i4 := make([]byte, 0, len(i3)+2)
	i4 = append(i4, i3...)
	i4 = append(i4, "  "...)
	if 0 < len(fi.Doc.Text) {
		b = append(b, indent...)
		b = append(b, "Documentation:\n"...)
		b = f.appendDoc(b, fi.Doc.Text, string(i2), 80, ansi)
		b = append(b, fi.Doc.Text...)
		b = append(b, '\n')
		for _, da := range fi.Doc.Args {
			if da.Name[0] == '&' {
				continue
			}
			b = append(b, i3...)
			if ansi {
				b = append(b, underline...)
				b = append(b, da.Name...)
				b = append(b, colorOff...)
			} else {
				b = append(b, da.Name...)
			}
			b = append(b, ": "...)
			b = append(b, da.Type...)
			if 0 < len(da.Text) {
				b = append(b, '\n')
				b = f.appendDoc(b, da.Text, string(i4), 80, ansi)
			}
			b = append(b, '\n')
		}
	}
	if 0 < len(fi.Doc.Examples) {
		b = append(b, '\n')
		b = append(b, indent...)
		b = append(b, "Examples:\n"...)
		for _, ex := range fi.Doc.Examples {
			b = append(b, i2...)
			b = append(b, ex...)
			b = append(b, '\n')
		}
	}
	return append(b, '\n')
}

func (f *Describe) appendDoc(b []byte, text string, indent string, width int, ansi bool) []byte {

	b = append(b, indent...)
	b = append(b, text...)

	// TBD

	return b
}
