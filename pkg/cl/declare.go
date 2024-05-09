// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Declare{Function: slip.Function{Name: "declare", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "declare",
			Args: []*slip.DocArg{
				{
					Name: "specifiers",
					Type: "object",
					Text: "Specifiers not evaluated.",
				},
			},
			Text: `__declare__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Declare represents the declare function.
type Declare struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Declare) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return slip.Novalue
}
