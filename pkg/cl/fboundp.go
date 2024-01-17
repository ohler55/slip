// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Fboundp{Function: slip.Function{Name: "fboundp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fboundp",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__fboundp__ returns _true_ if _object_ is function otherwise nil is returned.`,
			Examples: []string{
				"(fboundp 'x) => nil",
				"(setq x 3) (fboundp 'x) => t",
			},
		}, &slip.CLPkg)
}

// Fboundp represents the fboundp function.
type Fboundp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Fboundp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.PanicType("symbol", args[0], "symbol")
	}
	if slip.CurrentPackage.GetFunc(string(sym)) != nil {
		return slip.True
	}
	return nil
}
