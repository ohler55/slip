// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pp"
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
				"(disassemble 'quux)",
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
	a := args[0]
Top:
	switch ta := a.(type) {
	case slip.Symbol:
		if fi := slip.FindFunc(string(ta)); fi != nil {
			a = fi
			goto Top
		}
		slip.PanicType("fn", ta, "function designator")
	case *slip.Lambda:
		buf = f.disassembleLambda(s, ta, right, ansi)
	case *slip.FuncInfo:
		buf = f.disassembleFunction(s, ta, right, ansi)
	default:
		slip.PanicType("fn", ta, "function designator")
	}
	_, _ = w.Write(buf)

	return nil
}

func (f *Disassemble) disassembleFunction(s *slip.Scope, fi *slip.FuncInfo, right int, ansi bool) (b []byte) {
	s2 := s.NewScope()
	s2.UnsafeLet(slip.Symbol("*print-right-margin*"), slip.Fixnum(right))

	return pp.Append(nil, s2, fi)
}

func (f *Disassemble) disassembleLambda(s *slip.Scope, lam *slip.Lambda, right int, ansi bool) (b []byte) {
	b = append(b, "(lambda ("...)
	for i, da := range lam.Doc.Args {
		if 0 < i {
			b = append(b, ' ')
		}
		b = append(b, da.Name...)
	}
	b = append(b, ')', '\n')
	p := slip.DefaultPrinter()
	p.ScopedUpdate(s)
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
