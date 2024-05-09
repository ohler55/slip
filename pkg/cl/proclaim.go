// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Proclaim{Function: slip.Function{Name: "proclaim", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "proclaim",
			Args: []*slip.DocArg{
				{
					Name: "specifiers",
					Type: "object",
					Text: "Specifiers not evaluated.",
				},
			},
			Text: `__proclaim__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Proclaim represents the proclaim function.
type Proclaim struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Proclaim) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return slip.Novalue
}
