// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ignorable{Function: slip.Function{Name: "ignorable", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ignorable",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "Names are not evaluated.",
				},
			},
			Text: `__ignorable__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Ignorable represents the ignorable function.
type Ignorable struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Ignorable) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic(slip.NewUndefinedFunction(slip.Symbol("ignorable"),
		"is not a defined function. I can only be used as a declaration specifier."))
}
