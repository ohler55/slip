// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Psetf{Function: slip.Function{Name: "psetf", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "psetf",
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
			Text: `__psetf__ the value of the _symbol_ to _value_. Note that _symbol_ is not evaluated.
Repeated pairs of _symbol_ and _value_ are supported. All operations are completed in apparent parallel.`,
			Examples: []string{
				"(psetf) => nil",
				"(psetf x 7) => 7",
				"(psetf x 7 y 8) => 8",
			},
		}, &slip.CLPkg)
}

// Psetf represents the psetf function.
type Psetf struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Psetf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args)%2 != 0 {
		slip.NewPanic("psetf expected placer/value pairs. Not %d arguments.", len(f.Args))
	}
	d2 := depth + 1
	results := make(slip.List, len(args)/2)
	for i := 1; i < len(args); i += 2 {
		result = slip.EvalArg(s, args, i, d2)
		if vs, ok := result.(slip.Values); ok {
			result = vs.First()
		}
		results[i/2] = result
	}
	for i := 0; i < len(args)-1; i += 2 {
		p := args[i]
		result = results[i/2]
	Retry:
		switch ta := p.(type) {
		case slip.Symbol:
			s.Set(ta, result)
		case slip.List:
			p = slip.ListToFunc(s, ta, d2)
			goto Retry
		case slip.Placer:
			targs := ta.GetArgs()
			pargs := make(slip.List, len(targs))
			for j, v := range targs {
				if list, ok := v.(slip.List); ok {
					v = slip.ListToFunc(s, list, d2)
				}
				pargs[j] = s.Eval(v, d2)
			}
			ta.Place(pargs, result)
		default:
			slip.PanicType("placer argument to psetf", p, "symbol", "placer")
		}
	}
	return
}
