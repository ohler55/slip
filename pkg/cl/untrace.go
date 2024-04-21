// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Untrace{Function: slip.Function{Name: "untrace", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "untrace",
			Args: []*slip.DocArg{
				{
					Name: "name*",
					Type: "symbol",
					Text: "Function names to untrace.",
				},
			},
			Return: "list",
			Text: `__untrace__ turns tracing off for the listed function _name*. If no function
names are provided then tracing is turned off for all functions.`,
			Examples: []string{
				`(untrace t) ;; tracing on`,
				`(ununtrace) ;; tracing off`,
			},
		}, &slip.CLPkg)
}

// Untrace represents the untrace function.
type Untrace struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Untrace) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.Untrace(args)

	return nil
}
