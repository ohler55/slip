// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MultipleValueSetq{
				Function: slip.Function{Name: "multiple-value-setq", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "multiple-value-setq",
			Args: []*slip.DocArg{
				{
					Name: "vars",
					Type: "list",
					Text: "A list of variables to bind to the results of _form_.",
				},
				{
					Name: "form",
					Type: "form",
					Text: "The form to evaluate for values that the _vars_ can be bound to.",
				},
			},
			Return: "object",
			Text: `__multiple-value-setq__ binds the _vars_ to the result of evaluating _form_.
If there are more vars than values returned, nil is assigned to the extra vars. If there are
more values than vars, the extra values are discarded.`,
			Examples: []string{
				"(let (x y) (multiple-value-setq (x y) (floor 130 11)) (list x y)) => (11 9)",
			},
		}, &slip.CLPkg)
}

// MultipleValueSetq represents the multiple-value-setq function.
type MultipleValueSetq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MultipleValueSetq) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	list, ok := args[0].(slip.List)
	if !ok {
		slip.TypePanic(s, depth, "vars", args[0], "list of symbols")
	}
	form := args[1]
	d2 := depth + 1
	if lst, ok := form.(slip.List); ok {
		form = slip.ListToFunc(s, lst, d2)
	}
	v := s.Eval(form, d2)
	values, ok := v.(slip.Values)
	if !ok {
		values = slip.Values{v}
	}
	var (
		sym slip.Symbol
		i   int
	)
	for i, v = range list {
		if sym, ok = v.(slip.Symbol); !ok {
			slip.TypePanic(s, depth, "vars", v, "list of symbols")
		}
		if i < len(values) {
			s.Set(sym, values[i])
		} else {
			s.Set(sym, nil)
		}
	}
	return values[0]
}
