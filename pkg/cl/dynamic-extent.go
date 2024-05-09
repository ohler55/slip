// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DynamicExtent{Function: slip.Function{Name: "dynamic-extent", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "dynamic-extent",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "Names are not evaluated.",
				},
			},
			Text: `__dynamic-extent__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// DynamicExtent represents the dynamic-extent function.
type DynamicExtent struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DynamicExtent) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	panic(slip.NewUndefinedFunction(slip.Symbol("dynamic-extent"),
		"is not a defined function. I can only be used as a declaration specifier."))
}
