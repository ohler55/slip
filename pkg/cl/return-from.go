// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

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

// Call the the function with the arguments provided.
func (f *ReturnFrom) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	rr := ReturnResult{}
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.Symbol:
		rr.Tag = ta
	default:
		slip.PanicType("name", ta, "symbol", "nil")
	}
	if !s.Block {
		panic(fmt.Sprintf("return from unknown block: %s", rr.Tag))
	}
	rr.Result = f.EvalArg(s, args, 1, depth+1)

	return &rr
}
