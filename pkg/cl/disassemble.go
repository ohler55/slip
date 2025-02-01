// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Disassemble{Function: slip.Function{Name: "disassemble", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.FunctionSymbol,
			Name: "disassemble",
			Args: []*slip.DocArg{
				{
					Name: "fn",
					Type: "function",
					Text: "A function designator.",
				},
			},
			Return: "nil",
			Text:   `__disassemble__ prints details about the functions.`,
			Examples: []string{
				"(defun quux (x) (+ x 3))",
				"(disassemble 'quux",
				";; prints:",
				"(defun quux (x)",
				"  (+ 3 x))",
			},
		}, &slip.CLPkg)
}

// Disassemble represents the disassemble function.
type Disassemble struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Disassemble) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	ansi := s.Get("*print-ansi*") != nil
	right := int(s.Get("*print-right-margin*").(slip.Fixnum))
	w := s.Get("*standard-output*").(io.Writer)
	var buf []byte
	switch ta := args[0].(type) {
	case slip.Symbol:
		buf = f.disassembleSymbol(string(ta), right, ansi)
	case *slip.Lambda:
		buf = f.disassembleLambda(ta, right, ansi)
	default:
		slip.PanicType("fn", ta, "function designator")
	}
	_, _ = w.Write(buf)

	return nil
}

func (f *Disassemble) disassembleSymbol(name string, right int, ansi bool) (b []byte) {
	fi := slip.FindFunc(name)
	if fi == nil {
		slip.PanicType("fn", slip.Symbol(name), "function designator")
	}
	if fi.Kind == slip.Symbol("macro") {
		b = append(b, "(defmacro "...)
	} else {
		b = append(b, "(defun "...)
	}
	b = append(b, fi.Pkg.Name...)
	b = append(b, ':')
	b = append(b, fi.Name...)
	b = append(b, ' ', '(')
	for i, da := range fi.Doc.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		b = append(b, da.Name...)
	}
	b = append(b, ')', '\n')
	if 0 < len(fi.Doc.Text) {

		b = append(b, "  \""...)
		b2 := slip.AppendDoc(nil, fi.Doc.Text, 3, right, ansi)
		b = append(b, b2[3:]...)
		b = append(b, '"', '\n')
	}
	fun := fi.Create(nil).(slip.Funky)
	p := slip.DefaultPrinter()
	p.RightMargin = uint(right)
	p.ANSI = ansi
	if lam, ok := fun.Caller().(*slip.Lambda); ok {
		for _, form := range lam.Forms {
			b = append(b, ' ', ' ')
			b = p.Append(b, form, 1)
			b = append(b, '\n')
		}
		b[len(b)-1] = ')'
		b = append(b, '\n')
	} else {
		b = append(b, "  ...)\n"...)
	}
	return
}

func (f *Disassemble) disassembleLambda(lam *slip.Lambda, right int, ansi bool) (b []byte) {
	b = append(b, "(lambda ("...)
	for i, da := range lam.Doc.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		b = append(b, da.Name...)
	}
	b = append(b, ')', '\n')
	p := slip.DefaultPrinter()
	p.RightMargin = uint(right)
	p.ANSI = ansi
	for _, form := range lam.Forms {
		b = append(b, ' ', ' ')
		b = p.Append(b, form, 1)
		b = append(b, '\n')
	}
	b[len(b)-1] = ')'
	b = append(b, '\n')

	return
}
