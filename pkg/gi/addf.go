// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Addf{Function: slip.Function{Name: "addf", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "addf",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol bound to a list to be appended to.",
				},
				{Name: "&rest"},
				{
					Name: "objects",
					Type: "object",
					Text: "The objects to be appended to the _symbol_ list.",
				},
			},
			Return: "list",
			Text:   `__addf__ appends to the list the _symbol_ is bound to. __addf__ is an addition to common LISP.`,
			Examples: []string{
				"(setq lst '(a b))",
				"(addf lst 'c 'd) => (a b c d)",
				"lst => (a b c d)",
			},
		}, &Pkg)
}

// Addf represents the addf function.
type Addf struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Addf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol", args[0], "symbol")
	}
	var list slip.List
	switch tv := s.Get(sym).(type) {
	case nil:
		list = args[1:]
	case slip.List:
		list = append(tv, args[1:]...)
	default:
		slip.PanicType("symbol", tv, "list")
	}
	s.Set(sym, list)

	return list
}
