// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UnwindProtect{
				Function: slip.Function{Name: "unwind-protect", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "unwind-protect",
			Args: []*slip.DocArg{
				{
					Name: "protected-form",
					Type: "form",
					Text: "A form to evaluate.",
				},
				{Name: "&rest"},
				{
					Name: "cleanup-forms",
					Type: "form",
					Text: "The forms to evaluate after the protected-form.",
				},
			},
			Return: "object",
			Text: `__unwind-protect__ evaluates _protected-form_ and guarantees the _cleanup-forms_
are evaluated before exiting the __unwind-protect__ function.`,
			Examples: []string{
				"(let ((x 1)) (unwind-protect (setq x (/ 1 0)) (setq x 3) x) => 3",
			},
		}, &slip.CLPkg)
}

// UnwindProtect represents the unwind-protect function.
type UnwindProtect struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UnwindProtect) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	d2 := depth + 1
	defer func() {
		for i := 1; i < len(args); i++ {
			_ = slip.EvalArg(s, args, i, d2)
		}
	}()
	return slip.EvalArg(s, args, 0, d2)
}
