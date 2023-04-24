// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Backquote{Function: slip.Function{Name: "backquote", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "backquote",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "Any object.",
				},
			},
			Return: "object",
			Text: `__backquote__ returns _value_ without evaluating it except in the case where an
element of a backquoted list includes a __,__. TBD explain use of , ,@ and ',`,
			Examples: []string{
				"(backquote nil) => nil",
				"(backquote (a . b)) => (a . b)",
				"`(a b) => (a b)",
			},
		}, &slip.CLPkg)
}

// Backquote represents the backquote function.
type Backquote struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Backquote) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	return f.expand(s, args[0], depth)
}

func (f *Backquote) expand(s *slip.Scope, arg slip.Object, depth int) slip.Object {
	switch ta := arg.(type) {
	case slip.List:
		if 0 < len(ta) {
			xl := make(slip.List, len(ta))
			for i, a := range ta {
				xl[i] = f.expand(s, a, depth)
			}
			arg = xl
		}
	case slip.Funky:
		if obj, ok := arg.(slip.Object); ok {
			arg = obj.Eval(s, depth+1)
		}
	}
	return arg
}

// String representation of the Object.
func (f *Backquote) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Backquote) Append(b []byte) (out []byte) {
	if 0 < len(f.Args) {
		b = append(b, '`')
		out = slip.Append(b, f.Args[0])
	}
	return
}

// SpecialChar returns the prefix character for writing.
func (f *Backquote) SpecialChar() byte {
	return '`'
}
