// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Psetq{Function: slip.Function{Name: "psetq", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "psetq",
			Args: []*slip.DocArg{
				{
					Name: "symbol",
					Type: "symbol",
					Text: "The symbol to bind to the _value_.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to assign to _symbol.",
				},
			},
			Return: "object",
			Text: `__set__ the value of the _symbol_ to _value_. Note that _symbol_ is not evaluated.
Repeated pairs of _symbol_ and _value_ are supported. Bindings occur in parallel.`,
			Examples: []string{
				"(psetq) => nil",
				"(psetq x 7) => 7",
				"(psetq x 9 y (1+ x)) => 8 ; x was 7 from last assignment",
			},
		}, &slip.CLPkg)
}

// Psetq represents the psetq function.
type Psetq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Psetq) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args)%2 != 0 {
		slip.ErrorPanic(s, depth, "psetq expected symbol value pairs. Not %d arguments.", len(f.Args))
	}
	d2 := depth + 1
	last := len(args) - 1
	cnt2 := len(args) / 2
	syms := make([]slip.Symbol, 0, cnt2)
	vals := make(slip.List, 0, cnt2)
	for i := 0; i < last; i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "symbol argument to psetq", args[i], "symbol")
		}
		i++
		result = slip.EvalArg(s, args, i, d2)
		syms = append(syms, sym)
		vals = append(vals, result)
	}
	for i := cnt2 - 1; 0 <= i; i-- {
		s.Set(syms[i], vals[i])
	}
	return
}
