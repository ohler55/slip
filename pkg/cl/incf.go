// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Incf{Function: slip.Function{Name: "incf", Args: args, SkipEval: []bool{true, false}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "incf",
			Args: []*slip.DocArg{
				{
					Name: "place",
					Type: "placer",
					Text: "The place to increment.",
				},
				{
					Name: "delta-form",
					Type: "object",
					Text: "The delta amount to increment the _place_ value.",
				},
			},
			Return: "number",
			Text:   `__incf__ increments the value in _place_.`,
			Examples: []string{
				"(let ((x 0)) (incf x)) => 1",
				"(let ((x 0)) (incf x 1.5)) => 1.5",
				"(let ((x (list 1 2))) (incf (car x)) x) => (2 2)",
			},
		}, &slip.CLPkg)
}

// Incf represents the incf function.
type Incf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Incf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	var delta slip.Object = slip.Fixnum(1)
	if 1 < len(args) {
		delta = args[1]
	}
	d2 := depth + 1
	p := args[0]
Retry:
	switch tp := p.(type) {
	case slip.Symbol:
		result = addNumbers(s.Get(tp), delta)
		s.Set(tp, result)
	case slip.List:
		p = slip.ListToFunc(s, tp, d2)
		goto Retry
	case slip.Placer:
		targs := tp.GetArgs()
		pargs := make(slip.List, len(targs))
		for j, v := range targs {
			if list, ok := v.(slip.List); ok {
				v = slip.ListToFunc(s, list, d2)
			}
			pargs[j] = s.Eval(v, d2)
		}
		result = addNumbers(tp.Apply(s, pargs, d2), delta)
		tp.Place(s, pargs, result)
	default:
		slip.TypePanic(s, depth, "incf placer", tp, "placer", "symbol")
	}
	return
}
