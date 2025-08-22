// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Constantly{Function: slip.Function{Name: "constantly", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "constantly",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "The value the returned function should return.",
				},
			},
			Return: "function",
			Text:   `__constantly__ returns a function that always return _value_.`,
			Examples: []string{
				"(constantly 4) => #<function (lambda (&rest arguments)) {12345}>",
				"(mapcar (constantly 5) '(1 2 3)) => (5 5 5)",
			},
		}, &slip.CLPkg)
}

// Constantly represents the constantly function.
type Constantly struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Constantly) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	return &slip.Lambda{
		Doc: &slip.FuncDoc{
			Return: "object",
			Args: []*slip.DocArg{
				{Name: slip.AmpRest},
				{Name: "arguments"},
			},
			Kind: slip.LambdaSymbol,
		},
		Forms: slip.List{args[0]},
	}
}
