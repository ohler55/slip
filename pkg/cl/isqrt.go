// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Isqrt{Function: slip.Function{Name: "isqrt", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "isqrt",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "real",
					Text: "The number to determine the square root of.",
				},
			},
			Return: "integer",
			Text:   `__isqrt__ returns then square root of _number_ rounded down to the nearest integer.`,
			Examples: []string{
				"(isqrt 10) => 3",
			},
		}, &slip.CLPkg)
}

// Isqrt represents the isqrt function.
type Isqrt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Isqrt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.Bignum:
		result = (*slip.Bignum)((*big.Int)(ta).Sqrt((*big.Int)(ta)))
	case *slip.LongFloat:
		var z big.Int
		bi, _ := (*big.Float)(ta).Sqrt((*big.Float)(ta)).Int(&z)
		result = (*slip.Bignum)(bi)
	case slip.Real:
		rv := ta.RealValue()
		if rv < 0.0 {
			slip.PanicArithmetic(f, args, "only non-negative values are allowed")
		}
		result = slip.Fixnum(math.Sqrt(rv))
	default:
		slip.PanicType("number", args[0], "real")
	}
	return
}
