// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Abs{Function: slip.Function{Name: "abs", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "abs",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number take the absolute value of.",
				},
			},
			Return: "nil",
			Text: `__abs__ returns absolute value of _number_.
Unlike SBCL a complex number is not converted to a float.`,
			Examples: []string{
				"(abs 4) => 4",
				"(abs -5.2) => 5.2",
				"(abs #c(-1 2)) => #c(1 2)",
			},
		}, &slip.CLPkg)
}

// Abs represents the abs function.
type Abs struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Abs) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	result = args[0]
	switch ta := result.(type) {
	case slip.Fixnum:
		if ta < 0 {
			result = -ta
		}
	case slip.SingleFloat:
		if ta < 0.0 {
			result = -ta
		}
	case slip.DoubleFloat:
		if ta < 0.0 {
			result = -ta
		}
	case *slip.Ratio:
		var z big.Rat
		_ = z.Abs((*big.Rat)(ta))
		result = (*slip.Ratio)(&z)
	case *slip.Bignum:
		var z big.Int
		_ = z.Abs((*big.Int)(ta))
		result = (*slip.Bignum)(&z)
	case *slip.LongFloat:
		var z big.Float
		_ = z.Abs((*big.Float)(ta))
		result = (*slip.LongFloat)(&z)
	case slip.Complex:
		r := real(ta)
		i := imag(ta)
		if r < 0.0 {
			r = -r
		}
		if i < 0.0 {
			i = -i
		}
		result = slip.Complex(complex(r, i))
	default:
		slip.TypePanic(s, depth, "number", ta, "number")
	}
	return
}
