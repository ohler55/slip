// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Go{Function: slip.Function{Name: "go", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "go",
			Args: []*slip.DocArg{
				{
					Name: "tag",
					Type: "symbol|integer",
					Text: "The tag to go to in a tagbody.",
				},
			},
			Return: "",
			Text:   `__go__ jumps ahead in the body to the designated _tag_.`,
			Examples: []string{
				"(tagbody (setq x 1) (go skip) (setq x (1+ x)) skip (setq x (1+ x))) => 2",
			},
		}, &slip.CLPkg)
}

// Go represents the go function.
type Go struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Go) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	gt := GoTo{}
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.Symbol, slip.Integer:
		gt.Tag = ta
	default:
		if ta != slip.True {
			slip.TypePanic(s, depth, "name", ta, "symbol", "nil")
		}
		gt.Tag = ta
	}
	if !s.TagBody {
		slip.PanicControl("attempt to go to nonexistent tag: %s", gt.Tag)
	}
	return &gt
}
