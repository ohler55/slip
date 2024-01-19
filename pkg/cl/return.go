// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Return{Function: slip.Function{Name: "return", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "return",
			Args: []*slip.DocArg{
				{
					Name: "result",
					Type: "object",
					Text: "The result to return.",
				},
			},
			Return: "object",
			Text:   `__return-__ the _result_ from a block with a _nil_ name.`,
			Examples: []string{
				"(block nil (return 3)) => 3",
			},
		}, &slip.CLPkg)
}

// Return represents the return function.
type Return struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Return) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	rr := ReturnResult{}
	if !s.InBlock(nil) {
		slip.PanicControl("return from unknown block: nil")
	}
	if 0 < len(args) {
		rr.Result = slip.EvalArg(s, args, 0, depth+1)
	}
	return &rr
}
