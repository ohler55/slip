// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CommaAt{Function: slip.Function{Name: "comma-at", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "comma-at",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "Any object.",
				},
			},
			Return: "object",
			Text: `__commaAt__ is only allowed within a backquote. It evaluates and returns the value
of it's argument despite being in a backquoted expression.`,
			Examples: []string{
				"(setq x 3)",
				"`(commaAt x) => 3",
				"`(1 ,x) => (1 2)",
			},
		}, &slip.CLPkg)
}

// CommaAt represents the commaAt function.
type CommaAt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CommaAt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case nil:
		result = atList{}
	case slip.List:
		result = atList(ta)
	default:
		result = slip.Tail{Value: ta}
	}
	return
}

// String representation of the Object.
func (f *CommaAt) String() string {
	return string(f.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (f *CommaAt) Append(b []byte) (out []byte) {
	if 0 < len(f.Args) {
		b = append(b, ',', '@')
		out = slip.Append(b, f.Args[0])
	}
	return
}

// SpecialPrefix returns the prefix character for writing.
func (f *CommaAt) SpecialPrefix() string {
	return ",@"
}
