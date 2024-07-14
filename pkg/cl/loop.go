// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Loop{Function: slip.Function{Name: "loop", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "loop",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: `Forms to evaluate on each iteraction. A __return__ can be called
to exit the loop.`,
				},
			},
			Return: "object",
			Text:   `__loop__ iterates over a set of _forms_ until a __return__ statement is invoked.`,
			Examples: []string{
				`(let ((x 0))`,
				` (loop`,
				`  (setq x (1+ x))`,
				`  (when (< 3 x) (return x)))) => 4`,
			},
		}, &slip.CLPkg)
}

// Loop represents the loop function.
type Loop struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Loop) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	d2 := depth + 1
	ns := s.NewScope()
	ns.Block = true
	ns.TagBody = true
	for i, form := range args {
		if list, ok := form.(slip.List); ok {
			args[i] = slip.ListToFunc(ns, list, d2)
		}
	}
top:
	for {
		for _, form := range args {
			if tr, ok := ns.Eval(form, d2).(*slip.ReturnResult); ok {
				if tr.Tag == nil {
					result = tr.Result
					break top
				}
				if s.Block {
					result = tr
					break top
				}
				// slip.NewPanic("return from unknown block: %s", tr.Tag)
			}
			// Anything other than ReturnResult continues.
		}
	}
	return
}
