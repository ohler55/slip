// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Boundp{Function: slip.Function{Name: "boundp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "boundp",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__boundp__ returns _true_ if _object_ is bound otherwise nil is returned.`,
			Examples: []string{
				"(boundp 'x) => nil",
				"(setq x 3) (boundp 'x) => t",
			},
		}, &slip.CLPkg)
}

// Boundp represents the boundp function.
type Boundp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Boundp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol", args[0], "symbol")
	}
	if s.Bound(sym) {
		return slip.True
	}
	return nil
}
