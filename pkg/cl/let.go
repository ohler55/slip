// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Let{Function: slip.Function{Name: "let", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "let",
			Args: []*slip.DocArg{
				{
					Name: "bindings",
					Type: "list",
					Text: "A list of variables with optional initial value form.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__let__ binds the binding variables and then evaluates the forms in order with
a closure that includes the bindings. All bindings are performed in parallel unlike __let*__.`,
			Examples: []string{
				"(let ()) => nil",
				"(let ((x 1) y) (list x y)) => (1 nil)",
			},
		}, &slip.CLPkg)
}

// Let represents the let function.
type Let struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Let) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	ns := s.NewScope()
	d2 := depth + 1
	processBinding(s, ns, args[0], d2)
	for i := 1; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
		switch result.(type) {
		case *ReturnResult, *GoTo:
			return result
		}
	}
	return
}
