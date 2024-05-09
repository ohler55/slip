// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Declaration{Function: slip.Function{Name: "declaration", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "declaration",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "Names are not evaluated.",
				},
			},
			Text: `__declaration__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Declaration represents the declaration function.
type Declaration struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Declaration) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic(slip.NewUndefinedFunction(slip.Symbol("declaration"),
		"is not a defined function. I can only be used as a declaration specifier."))
}
