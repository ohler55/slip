// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Letx{Function: slip.Function{Name: "let*", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "let*",
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
			Text: `__let*__ binds the binding variables and then evaluates the forms in order with
a closure that includes the bindings. All bindings are performed in sequence unlike __let__.`,
			Examples: []string{
				"(let* ()) => nil",
				"(let* ((x 1) (y x)) (list x y)) => (1 1)",
			},
		}, &slip.CLPkg)
}

// Letx represents the let* function.
type Letx struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Letx) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	bindings, ok := args[0].(slip.List)
	if !ok {
		slip.PanicType("let* bindings", args[0], "list")
	}
	ns := s.NewScope()
	d2 := depth + 1
	for _, binding := range bindings {
		switch tb := binding.(type) {
		case slip.Symbol:
			ns.Let(tb, nil)
		case slip.List:
			if len(tb) < 1 {
				slip.PanicType("let* local variable binding", nil, "list", "symbol")
			}
			var sym slip.Symbol
			if sym, ok = tb[0].(slip.Symbol); !ok {
				slip.PanicType("let* local variable binding", tb[0], "symbol")
			}
			if 1 < len(tb) {
				// Use the original scope to avoid using the new bindings since
				// they are evaluated in apparent parallel.
				ns.Let(sym, slip.EvalArg(ns, tb, 1, d2))
			} else {
				ns.Let(sym, nil)
			}
		default:
			slip.PanicType("let* binding", f, "list", "symbol")
		}
	}
	for i := 1; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
		switch result.(type) {
		case *slip.ReturnResult, *GoTo:
			return result
		}
	}
	return
}
