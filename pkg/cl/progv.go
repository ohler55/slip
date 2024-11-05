// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Progv{Function: slip.Function{Name: "progv", Args: args, SkipEval: []bool{false, false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "progv",
			Args: []*slip.DocArg{
				{
					Name: "symbols",
					Type: "list",
					Text: "The symbols to bind values to.",
				},
				{
					Name: "values",
					Type: "list",
					Text: "The value that the symbols are bound to.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__progv__ binds _symbols_ to _values_ similar to __let__ and evaluates
all _forms_ in that scope, returning the value of the last form. Deviating from Common LISP,
the bindings are not hidden in symbol bindings only accessed by __symbol-value__ but are
bound in the scope of the __progv__ function.`,
			Examples: []string{
				"(progv '(x) '(3) (1+ x)) => 4",
			},
		}, &slip.CLPkg)
}

// Progv represents the progv function.
type Progv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Progv) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	ns := s.NewScope()
	var (
		symbols slip.List
		values  slip.List
	)
	switch ta := args[0].(type) {
	case nil:
		// leave list empty
	case slip.List:
		symbols = ta
	default:
		slip.PanicType("symbols", ta, "list")
	}
	switch ta := args[1].(type) {
	case nil:
		// leave list empty
	case slip.List:
		values = ta
	default:
		slip.PanicType("values", ta, "list")
	}
	for i, v := range symbols {
		if sym, ok := v.(slip.Symbol); ok {
			if i < len(values) {
				ns.Let(sym, values[i])
			} else {
				ns.Let(sym, slip.Unbound)
			}
		} else {
			slip.PanicType("symbol element", v, "list of symbols")
		}
	}
	d2 := depth + 1
	for i := 2; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
	}
	return
}
