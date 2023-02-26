// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Setq{Function: slip.Function{Name: "setq", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "setq",
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
Repeated pairs of _symbol_ and _value_ are supported`,
			Examples: []string{
				"(setq) => nil",
				"(setq x 7) => 7",
				"(setq x 7 y 8) => 8",
			},
		}, &slip.CLPkg)
}

// Setq represents the setq function.
type Setq struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Setq) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args)%2 != 0 {
		panic(fmt.Sprintf("setq expected symbol value pairs. Not %d arguments.", len(f.Args)))
	}
	d2 := depth + 1
	for i := len(args) - 1; 0 <= i; i-- {
		sym, ok := args[i].(slip.Symbol)
		if !ok {
			slip.PanicType("symbol argument to setq", args[i], "symbol")
		}
		i--
		result = f.EvalArg(s, args, i, d2)
		s.Set(sym, result)
	}
	return
}
