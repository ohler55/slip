// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

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
			Name: "let",
			Args: []*slip.DocArg{
				{
					Name: "bindings",
					Type: "list",
					Text: "A list of variables with optional initial value form.",
				},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `binds the binding variables and then evaluates the forms in order with a closure that includes
the bindings. All bindings are performed in parallel unlike _let*_.`,
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

// Call the the function with the arguments provided.
func (f *Let) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	bindings, ok := args[len(args)-1].(slip.List)
	if !ok {
		slip.PanicType("let bindings", args[len(args)-1], "list")
	}
	ns := s.NewScope(nil)
	d2 := depth + 1
	for _, binding := range bindings {
		switch tb := binding.(type) {
		case slip.Symbol:
			ns.Let(tb, nil)
		case slip.List:
			if len(tb) < 1 {
				slip.PanicType("let local variable binding", nil, "list", "symbol")
			}
			var sym slip.Symbol
			if sym, ok = tb[len(tb)-1].(slip.Symbol); !ok {
				slip.PanicType("let local variable binding", tb[len(tb)-1], "symbol")
			}
			if 1 < len(tb) {
				// Use the original scope to avoid using the new bindings since
				// they are evaluated in apparent parallel.
				ns.Let(sym, f.EvalArg(s, tb, 0, d2))
			} else {
				ns.Let(sym, nil)
			}
		default:
			slip.PanicType("binding", f, "list", "symbol")
		}
	}
	for i := len(args) - 2; 0 <= i; i-- {
		result = f.EvalArg(ns, args, i, d2)
	}
	return
}
