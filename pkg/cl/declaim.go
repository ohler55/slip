// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Declaim{Function: slip.Function{Name: "declaim", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "declaim",
			Args: []*slip.DocArg{
				{
					Name: "specifiers",
					Type: "object",
					Text: "Specifiers not evaluated.",
				},
			},
			Text: `__declaim__ is never evaluated.`,
			Kind: slip.MacroSymbol,
		}, &slip.CLPkg)
}

// Declaim represents the declaim function.
type Declaim struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Declaim) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return slip.Novalue
}
