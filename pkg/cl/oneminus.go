// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Oneminus{Function: slip.Function{Name: "1-", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "1-",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to decrement.",
				},
			},
			Return: "number",
			Text:   `__1+__ returns the _number_ minus 1.`,
			Examples: []string{
				"(1- 5) => 4",
				"(1- 5/3) => 2/3",
				"(1- #c(1.0 1.0)) => #C(0 1)",
			},
		}, &slip.CLPkg)
}

// Oneminus represents the oneminus function.
type Oneminus struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Oneminus) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Fixnum:
		result = ta - 1
	case slip.SingleFloat:
		result = ta - 1.0
	case slip.DoubleFloat:
		result = ta - 1.0
	case *slip.LongFloat:
		var z big.Float
		return (*slip.LongFloat)(z.Add((*big.Float)(ta), big.NewFloat(-1.0)))
	case *slip.Bignum:
		var z big.Int
		return (*slip.Bignum)(z.Add((*big.Int)(ta), big.NewInt(-1)))
	case *slip.Ratio:
		var z big.Int
		den := (*big.Rat)(ta).Denom()
		num := z.Sub((*big.Rat)(ta).Num(), den)
		(*big.Rat)(ta).SetFrac(num, den)
		return ta
	case slip.Complex:
		result = slip.Complex(complex(real(ta)-1.0, imag(ta)))
	default:
		slip.PanicType("number", ta, "number")
	}
	return
}
