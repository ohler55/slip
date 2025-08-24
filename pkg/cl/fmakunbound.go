// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fmakunbound{Function: slip.Function{Name: "fmakunbound", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fmakunbound",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The symbol of make unbound.",
				},
			},
			Return: "symbol",
			Text:   `__fmakunbound__ makes the _symbol_ unbound if bound to a function.`,
			Examples: []string{
				"(defun 'quux () 5) => quux",
				"(fmakunbound 'quux) => quux",
				"(fboundp 'quux) => false",
			},
		}, &slip.CLPkg)
}

// Fmakunbound represents the fmakunbound function.
type Fmakunbound struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Fmakunbound) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "symbol", args[0], "symbol")
	}
	slip.CurrentPackage.Undefine(string(sym))

	return sym
}
