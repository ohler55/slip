// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Run{Function: slip.Function{Name: "run", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			// TBD or is it better to be (run (setq x 7)), different than apply but same as eval
			Kind: slip.MacroSymbol,
			Name: "run",
			Args: []*slip.DocArg{
				{
					Name: "form",
					Type: "object",
					Text: "A form to evaluate in a separate thread.",
				},
			},
			Return: "object",
			Text:   `__run__ evaluates a form in a separate thread (go routine).`,
			Examples: []string{
				`(run (setq x 7))`,
			},
		}, &Pkg)
}

// Run represents the run function.
type Run struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Run) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if args[0] != nil {
		go func() { _ = args[0].Eval(s, depth) }()
	}
	return slip.Novalue
}
