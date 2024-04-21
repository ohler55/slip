// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Trace{Function: slip.Function{Name: "trace", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "trace",
			Args: []*slip.DocArg{
				{
					Name: "name*",
					Type: "symbol",
					Text: "Function names to trace.",
				},
			},
			Return: "list",
			Text: `__trace__ turns tracing on for the listed function _name*. If the only name is _t_ then
tracing is turned on for all functions. If no functions are named then a list of the functions being
traced is returned.`,
			Examples: []string{
				`(trace t) ;; tracing on`,
				`(untrace) ;; tracing off`,
			},
		}, &slip.CLPkg)
}

// Trace represents the trace function.
type Trace struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Trace) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return slip.Trace(args)
}
