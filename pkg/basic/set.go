// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Set{Function: slip.Function{Name: "set", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "set",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to bind to the _value_.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to assign to _symbol.",
				},
			},
			Return: "object",
			Text:   `the value of the _symbol_ to _value_.`,
			Examples: []string{
				"(set 'x 7) => 7",
			},
		})
}

// Set represents the set function.
type Set struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Set) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol argument to set", args[1], "symbol")
	}
	s.Set(sym, args[0])

	return args[0]
}
