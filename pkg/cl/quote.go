// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Quote{Function: slip.Function{Name: "quote", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "quote",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "Any object.",
				},
			},
			Return: "object",
			Text:   `__quote__ returns _value_ without evaluating it.`,
			Examples: []string{
				"(quote nil) => nil",
				"(quote (a . b)) => (a . b)",
				"'(a b) => (a b)",
			},
		}, &slip.CLPkg)
}

// Quote represents the quote function.
type Quote struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Quote) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	return args[0]
}

// String representation of the Object.
func (f *Quote) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *Quote) Append(b []byte) (out []byte) {
	if 0 < len(f.Args) {
		b = append(b, '\'')
		out = slip.Append(b, f.Args[0])
	}
	return
}
