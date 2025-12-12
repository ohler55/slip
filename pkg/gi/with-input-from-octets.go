// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"bytes"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithInputFromOctets{
				Function: slip.Function{Name: "with-input-from-octets", Args: args, SkipEval: []bool{true}},
			}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "with-input-from-octets",
			Args: []*slip.DocArg{
				{
					Name: "args",
					Type: "list",
					Text: `A list of (var octets).`,
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "The forms to evaluate.",
				},
			},
			Return: "object",
			Text: `__with-input-from-octets__ evaluates the _forms_ after creating a stream from the
_octets_ provided.`,
			Examples: []string{
				`(with-input-from-octets (s "abc def") (read s)) => abc`,
			},
		}, &Pkg)
}

// WithInputFromOctets represents the with-input-from-octets function.
type WithInputFromOctets struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WithInputFromOctets) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	forms := args[1:]
	if list, ok := args[0].(slip.List); ok {
		args = list
	} else {
		slip.TypePanic(s, depth, "args", args[0], "list")
	}
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "args[0]", args[0], "symbol")
	}
	d2 := depth + 1
	args[1] = slip.EvalArg(s, args, 1, d2)
	data := []byte(slip.CoerceToOctets(args[1]).(slip.Octets))

	s2 := s.NewScope()
	s2.Let(sym, slip.NewInputStream(bytes.NewReader(data)))
	for i := range forms {
		result = slip.EvalArg(s2, forms, i, d2)
	}
	return
}
