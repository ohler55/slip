// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"bytes"
	"io"
	"strings"

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

// Call the function with the arguments provided.
func (f *Describe) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	obj := args[0]
	so := s.Get("*standard-output*")
	ss, _ := so.(slip.Stream)
	w := so.(io.Writer)
	if 1 < len(args) {
		var ok bool
		ss, _ = args[1].(slip.Stream)
		if w, ok = args[1].(io.Writer); !ok {
			slip.PanicType("describe output-stream", args[1], "output-stream")
		}
	}
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))

	b := AppendDescribe(nil, obj, s, 0, right, ansi)
	if _, err := w.Write(b); err != nil {
		slip.PanicStream(ss, "write failed: %s", err)
	}
	return slip.Novalue
}

// AppendDescribe appends a symbol description to a buffer.
func AppendDescribe(b []byte, obj slip.Object, s *slip.Scope, indent, right int, ansi bool) []byte {
	sym, ok := obj.(slip.Symbol)
	if !ok {
		if d, ok2 := obj.(slip.Describer); ok2 {
			b = d.Describe(b, indent, right, ansi)
		} else {
			b, _ = describeHead(b, nil, obj, indent, right, ansi)
		}
		return b
	}
	pkg := slip.CurrentPackage
	if strings.ContainsRune(string(sym), ':') {
		parts := strings.SplitN(string(sym), ":", 2)
		sym = slip.Symbol(parts[1])
		if pkg = slip.FindPackage(parts[0]); pkg == nil {
			slip.NewPanic("Package %s does not exist", parts[0])
		}
	}
	var pad []byte
	if v, has := s.LocalGet(sym); has {
		b, pad = describeHead(b, pkg, obj, indent, right, ansi)
		b = append(b, '\n')
		obj = v
		b = describeSymNames(b, sym, obj, pad, ansi)
	} else if fi := pkg.GetFunc(string(sym)); fi != nil {
		obj = fi
		if pkg != fi.Pkg {
			pkg = fi.Pkg
		}
		b, pad = describeHead(b, pkg, sym, indent, right, ansi)
		b = append(b, '\n')
		b = append(b, pad...)
		b = describeSymNames(b, sym, obj, pad, ansi)
	} else if vv := pkg.GetVarVal(strings.ToLower(string(sym))); vv != nil {
		obj = vv.Value()
		pkg = vv.Pkg
		b, pad = describeHead(b, pkg, sym, indent, right, ansi)
		b = append(b, '\n')
		b = append(b, pad...)
		b = describeSymNames(b, sym, obj, pad, ansi)
		if 0 < len(vv.Doc) {
			b = append(b, "  Documentation:\n"...)
			b = append(b, pad...)
			b = slip.AppendDoc(b, vv.Doc, indent+4, right, ansi)
			b = append(b, '\n')
			b = append(b, pad...)
		}
	} else {
		b, _ = describeHead(b, pkg, sym, indent, right, ansi)
		b = append(b, "  unbound\n"...)
		return b
	}
	indent += 2
	if d, ok := obj.(slip.Describer); ok {
		b = d.Describe(b, indent, right, ansi)
	} else {
		b = append(b, spaces[:indent]...)
		b = append(b, "Value = "...)
		b = slip.Append(b, obj)
		b = append(b, '\n')
	}
	return b
}

func describeHead(b []byte, pkg *slip.Package, obj slip.Object, indent, right int, ansi bool) (out, pad []byte) {
	if 0 < indent {
		pad = bytes.Repeat([]byte{' '}, indent)
	}
	b = append(b, pad...)
	if ansi {
		b = append(b, bold...)
		if pkg != nil {
			b = append(b, pkg.Name...)
			b = append(b, ':')
		}
		b = slip.Append(b, obj)
		b = append(b, colorOff...)
	} else {
		if pkg != nil {
			b = append(b, pkg.Name...)
			b = append(b, ':')
		}
		b = slip.Append(b, obj)
	}
	b = append(b, "\n  "...)
	b = append(b, pad...)
	b = append(b, '[')
	if obj == nil {
		b = append(b, "null"...)
	} else {
		b = append(b, string(obj.Hierarchy()[0])...)
	}
	b = append(b, "]\n"...)

	out = b
	return
}

func describeSymNames(b []byte, sym slip.Symbol, obj slip.Object, pad []byte, ansi bool) []byte {
	if ansi {
		b = append(b, bold...)
		b = slip.Append(b, sym)
		b = append(b, colorOff...)
	} else {
		b = slip.Append(b, sym)
	}
	b = append(b, " names a "...)
	if obj == nil {
		b = append(b, "null"...)
	} else {
		b = append(b, string(obj.Hierarchy()[0])...)
	}
	b = append(b, ":\n"...)
	b = append(b, pad...)
	return b
}
