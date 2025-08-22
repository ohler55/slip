// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Numerator{Function: slip.Function{Name: "numerator", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "numerator",
			Args: []*slip.DocArg{
				{
					Name: "rational",
					Type: "rational",
					Text: "The rational number to determine the numerator of.",
				},
			},
			Return: "integer",
			Text:   `__numerator__ returns then numerator of _rational_.`,
			Examples: []string{
				"(numerator 3) => 3",
				"(numerator 3/4) => 3",
			},
		}, &slip.CLPkg)
}

// Numerator represents the numerator function.
type Numerator struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Numerator) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum, *slip.Bignum:
		result = ta
	case *slip.Ratio:
		result = (*slip.Bignum)((*big.Rat)(ta).Num())
	default:
		slip.TypePanic(s, depth, "rational", args[0], "rational")
	}
	return
}
