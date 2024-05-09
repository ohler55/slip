// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ftype{Function: slip.Function{Name: "ftype", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ftype",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "Names are not evaluated.",
				},
			},
			Text: `__ftype__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Ftype represents the ftype function.
type Ftype struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Ftype) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic(slip.NewUndefinedFunction(slip.Symbol("ftype"),
		"is not a defined function. I can only be used as a declaration specifier."))
}
