// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sqrt{Function: slip.Function{Name: "sqrt", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sqrt",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to take the square root of.",
				},
			},
			Return: "nil",
			Text:   `__sqrt__ returns the square root of _number_.`,
			Examples: []string{
				"(sqrt 25) => 5",
				"(sqrt -9) => #C(0.0 3.0)",
			},
		}, &slip.CLPkg)
}

// Sqrt represents the sqrt function.
type Sqrt struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sqrt) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta < 0 {
			result = slip.Complex(complex(0.0, math.Sqrt(float64(-ta))))
		} else {
			result = slip.DoubleFloat(math.Sqrt(float64(ta)))
		}
	case slip.SingleFloat:
		if ta < 0 {
			result = slip.Complex(complex(0.0, math.Sqrt(float64(-ta))))
		} else {
			result = slip.DoubleFloat(math.Sqrt(float64(ta)))
		}
	case slip.DoubleFloat:
		if ta < 0 {
			result = slip.Complex(complex(0.0, math.Sqrt(float64(-ta))))
		} else {
			result = slip.DoubleFloat(math.Sqrt(float64(ta)))
		}
	case *slip.LongFloat:
		v := ta.RealValue()
		if v < 0.0 {
			result = slip.Complex(complex(0.0, math.Sqrt(-v)))
		} else {
			result = slip.DoubleFloat(math.Sqrt(v))
		}
	case *slip.Bignum:
		v := ta.RealValue()
		if v < 0.0 {
			result = slip.Complex(complex(0.0, math.Sqrt(-v)))
		} else {
			result = slip.DoubleFloat(math.Sqrt(v))
		}
	case *slip.Ratio:
		v := ta.RealValue()
		if v < 0.0 {
			result = slip.Complex(complex(0.0, math.Sqrt(-v)))
		} else {
			result = slip.DoubleFloat(math.Sqrt(v))
		}
	default:
		slip.TypePanic(s, depth, "number", ta, "number")
	}
	return
}
