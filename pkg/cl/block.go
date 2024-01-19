// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Block{Function: slip.Function{Name: "block", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "block",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of the block or _nil_.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text:   `__block__ names a set of forms.`,
			Examples: []string{
				"(block no-op) => nil",
				"(block a-name (+ 1 2) (+ 2 3)) => 5",
				"(block break (+ 1 2) (return-from break 1) (+ 2 3)) => 1",
			},
		}, &slip.CLPkg)
}

// Block represents the block function.
type Block struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Block) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	ns := s.NewScope()
	ns.Block = true
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.Symbol:
		ns.Name = ta
	default:
		slip.PanicType("name", ta, "symbol", "nil")
	}
	d2 := depth + 1
	for i := 1; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
		if rr, _ := result.(*ReturnResult); rr != nil {
			if ns.Name == rr.Tag {
				return rr.Result
			}
			return
		}
	}
	return
}
