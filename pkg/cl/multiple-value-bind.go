// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MultipleValueBind{Function: slip.Function{Name: "multiple-value-bind", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "multiple-value-bind",
			Args: []*slip.DocArg{
				{
					Name: "vars",
					Type: "list",
					Text: "A list of variables to bind to the results of _values-form_.",
				},
				{
					Name: "values-form",
					Type: "form",
					Text: "The form to evaluate for values that the _vars_ can be bound to.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "object",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__multiple-value-bind__ binds the _vars_ to the result of evaluating _values-form_
and then evaluates the _forms_ in order.`,
			Examples: []string{
				"(multiple-value-bind (x y) (floor 130 11) (list x y)) => (11 9)",
			},
		}, &slip.CLPkg)
}

// MultipleValueBind represents the multiple-value-bind function.
type MultipleValueBind struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MultipleValueBind) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	v := slip.EvalArg(s, args, 1, depth)
	values, ok := v.(slip.Values)
	if !ok {
		values = slip.Values{v}
	}
	ns := s.NewScope()
	var vars slip.List
	if vars, ok = args[0].(slip.List); !ok {
		slip.PanicType("vars", args[0], "list of symbols")
	}
	var i int
	for i, v = range vars {
		var sym slip.Symbol
		if sym, ok = v.(slip.Symbol); !ok {
			slip.PanicType("vars", v, "list of symbols")
		}
		if i < len(values) {
			ns.Let(sym, values[i])
		} else {
			ns.Let(sym, nil)
		}
	}
	d2 := depth + 1
	for i := 2; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
		switch result.(type) {
		case *slip.ReturnResult, *GoTo:
			return result
		}
	}
	return
}
