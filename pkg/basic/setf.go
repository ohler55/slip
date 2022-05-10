// Copyright (c) 2022, Peter Ohler, All rights reserved.

package basic

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
			Name: "setf",
			Args: []*slip.DocArg{
				{
					Name: "placer",
					Type: "placer",
					Text: "The symbol to bind to the _value_.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to assign to _symbol.",
				},
			},
			Return: "object",
			Text: `the value of the _symbol_ to _value_. Note that _symbol_ is not evaluated.
Repeated pairs of _symbol_ and _value_ are supported`,
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

// Call the the function with the arguments provided.
func (f *Setf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args)%2 != 0 {
		panic(fmt.Sprintf("setf expected placer/value pairs. Not %d arguments.", len(f.Args)))
	}
	d2 := depth + 1
	for i := len(args) - 1; 0 <= i; i-- {
		p := args[i]
		i--
		result = f.EvalArg(s, args, i, d2)
		if vs, ok := result.(slip.Values); ok {
			result = vs.First()
		}
	Retry:
		switch ta := p.(type) {
		case slip.Symbol:
			s.Set(ta, result)
		case slip.List:
			p = slip.ListToFunc(ta)
			goto Retry
		case slip.Placer:
			targs := ta.GetArgs()
			pargs := make(slip.List, len(targs))
			for j := len(targs) - 1; 0 <= j; j-- {
				v := targs[j]
				if list, ok := v.(slip.List); ok {
					v = slip.ListToFunc(list)
				}
				pargs[j] = s.Eval(v, d2)
			}
			ta.Place(pargs, result)
		default:
			slip.PanicType("placer argument to setf", p, "symbol", "placer")
		}
	}
	return
}
