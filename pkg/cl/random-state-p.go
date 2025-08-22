// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RandomStatep{Function: slip.Function{Name: "random-state-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "random-state-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__random-state-p__ returns _true_ if _object_ is a random-state.`,
			Examples: []string{
				"(random-state-p 4) => t",
				"(random-state-p 4/3) => t",
				"(random-state-p 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// RandomStatep represents the random-state-p function.
type RandomStatep struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RandomStatep) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if _, ok := args[0].(*RandomState); ok {
		return slip.True
	}
	return nil
}
