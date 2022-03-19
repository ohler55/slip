// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object { return &Set{Function: slip.Function{Name: "set", Args: args}} },
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

type Set struct {
	slip.Function
}

// Eval the object.
func (f *Set) Eval(s *slip.Scope, depth int) (result slip.Object) {
	if len(f.Args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	d2 := depth + 1
	s.Before(f, depth)
	arg := s.Eval(f.Args[0], d2)
	sym, ok := arg.(slip.Symbol)
	if !ok {
		slip.PanicType("symbol argument to set", arg, "symbol")
	}
	value := s.Eval(f.Args[1], d2)
	s.Set(sym, value)

	return value
}
