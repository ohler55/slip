// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Optimize{Function: slip.Function{Name: "optimize", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "optimize",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "Names are not evaluated.",
				},
			},
			Text: `__optimize__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Optimize represents the optimize function.
type Optimize struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Optimize) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic(slip.NewUndefinedFunction(slip.Symbol("optimize"),
		"is not a defined function. I can only be used as a declaration specifier."))
}
