// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReturnFrom{Function: slip.Function{Name: "return-from", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "return-from",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the block to return from.",
				},
				{
					Name: "result",
					Type: "object",
					Text: "The result to return.",
				},
			},
			Return: "object",
			Text:   `__return-from__ returns from the _name_ block with the _result_ provided.`,
			Examples: []string{
				"(block one (return-from one 3)) => 3",
			},
		}, &slip.CLPkg)
}

// ReturnFrom represents the returnFrom function.
type ReturnFrom struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReturnFrom) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	rr := slip.ReturnResult{}
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.Symbol:
		rr.Tag = ta
	default:
		slip.TypePanic(s, depth, "name", ta, "symbol", "nil")
	}
	if !s.InBlock(rr.Tag) {
		slip.ControlPanic(s, depth, "return from unknown block: %s", rr.Tag)
	}
	if 1 < len(args) {
		rr.Result = slip.EvalArg(s, args, 1, depth+1)
	}
	return &rr
}
