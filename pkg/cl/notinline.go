// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Notinline{Function: slip.Function{Name: "notinline", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "notinline",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "Names are not evaluated.",
				},
			},
			Text: `__notinline__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Notinline represents the notinline function.
type Notinline struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Notinline) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic(slip.UndefinedFunctionNew(s, depth, slip.Symbol("notinline"),
		"is not a defined function. I can only be used as a declaration specifier."))
}
