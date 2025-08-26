// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Setf{Function: slip.Function{Name: "setf", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "setf",
			Args: []*slip.DocArg{
				{
					Name: "place",
					Type: "place",
					Text: "The symbol or place to bind to the _value_.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to assign to _symbol.",
				},
			},
			Return: "object",
			Text: `__setf__ the value of the _place_ to _value_. Note that _place_ is not evaluated.
Repeated pairs of _place_ and _value_ are supported`,
			Examples: []string{
				"(setf) => nil",
				"(setf x 7) => 7",
				"(setf x 7 y 8) => 8",
			},
		}, &slip.CLPkg)
}

// Setf represents the setf function.
type Setf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Setf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args)%2 != 0 {
		slip.ErrorPanic(s, depth, "setf expected placer/value pairs. Not %d arguments.", len(f.Args))
	}
	d2 := depth + 1
	for i := 0; i < len(args)-1; i++ {
		p := args[i]
		i++
		result = slip.EvalArg(s, args, i, d2)
		if vs, ok := result.(slip.Values); ok {
			result = vs.First()
		}
		placeExprValue(s, p, result, d2)
	}
	return
}

func placeExprValue(s *slip.Scope, x slip.Object, value slip.Object, depth int) {
Retry:
	switch tx := x.(type) {
	case slip.Symbol:
		s.Set(tx, value)
	case slip.List:
		x = slip.ListToFunc(s, tx, depth)
		goto Retry
	case slip.Placer:
		callPlace(s, tx, value, depth)
	case slip.Funky:
		fargs := tx.GetArgs()
		if fi := slip.FindFunc(fmt.Sprintf("(setf %s)", tx.GetName())); fi != nil && 0 < len(fargs) {
			args := slip.List{fargs[0], value}
			f := fi.Create(args)
			f.Eval(s, depth)
			return
		}
		slip.TypePanic(s, depth, "placer argument to setf", x, "symbol", "placer")
	default:
		slip.TypePanic(s, depth, "placer argument to setf", x, "symbol", "placer")
	}
}

func callPlace(s *slip.Scope, p slip.Placer, value slip.Object, depth int) {
	targs := p.GetArgs()
	pargs := make(slip.List, len(targs))
	for j, v := range targs {
		if list, ok := v.(slip.List); ok {
			v = slip.ListToFunc(s, list, depth)
		}
		if !p.SkipArgEval(j) {
			pargs[j] = s.Eval(v, depth)
		} else {
			pargs[j] = v
		}
	}
	p.Place(s, pargs, value)
}
