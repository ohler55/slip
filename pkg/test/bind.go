// Copyright (c) 2026, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Bind{Function: slip.Function{Name: "bind", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "bind",
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
			Text: `__bind__ the _symbol_ to _value_ in the local scope similar to a __let__.
Note that _symbol_ is not evaluated. Repeated pairs of _symbol_ and _value_ are supported. This
function is particularly useful in a test suite setup function which shares a scope with the
test functions in the suite.`,
			Examples: []string{
				"(bind x 7) => 7",
				"(bind x 7 y 8) => 8",
			},
		}, &Pkg)
}

// Bind represents the bind function.
type Bind struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Bind) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args)%2 != 0 || len(args) == 0 {
		slip.ErrorPanic(s, depth, "bind expected symbol value pairs. Not %d arguments.", len(f.Args))
	}
	d2 := depth + 1
	last := len(args) - 1
	if pa := s.Parents(); 0 < len(pa) { // Should always be true.
		s = pa[0]
	}
	for i := 0; i < last; i++ {
		sym, ok := args[i].(slip.Symbol)
		if !ok {
			slip.TypePanic(s, depth, "symbol argument to bind", args[i], "symbol")
		}
		i++
		result = slip.EvalArg(s, args, i, d2)
		s.Let(sym, result)
	}
	return
}
