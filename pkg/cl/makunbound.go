// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Makunbound{Function: slip.Function{Name: "makunbound", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "makunbound",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol of make unbound.",
				},
			},
			Return: "symbol",
			Text:   `__makunbound__ makes the _symbol_ unbound if bound to a variable.`,
			Examples: []string{
				"(defvar 'quux 5) => 5",
				"(makunbound 'quux) => quux",
				"(boundp 'quux) => false",
			},
		}, &slip.CLPkg)
}

// Makunbound represents the makunbound function.
type Makunbound struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Makunbound) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	if !s.Remove(sym) {
		if !slip.CurrentPackage.Locked {
			slip.CurrentPackage.Remove(string(sym))
		}
	}
	return sym
}
