// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math"
	"math/cmplx"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Log{Function: slip.Function{Name: "log", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "log",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to return the logarithm of in the _base_.",
				},
				{Name: slip.AmpOptional},
				{
					Name: "base",
					Type: "number",
					Text: "The optional base for the logarithm.",
				},
			},
			Return: "nil",
			Text:   `__log__ returns the logarithm of _number_ in base _base_. The default is _e_.`,
			Examples: []string{
				"(log 2) => 0.6931472",
				"(log 2 10) => 0.30103",
			},
		}, &slip.CLPkg)
}

// Log represents the log function.
type Log struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Log) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	isE := true
	var baseArg slip.Object = slip.DoubleFloat(math.E)
	numArg := args[0]
	if 1 < len(args) {
		baseArg = args[1]
		isE = false
	}
	switch num := numArg.(type) {
	case slip.Real:
		rn := num.RealValue()
		switch base := baseArg.(type) {
		case slip.Real:
			rb := base.RealValue()
			switch rb {
			case 2.0:
				result = slip.DoubleFloat(math.Log2(rn))
			case 10.0:
				result = slip.DoubleFloat(math.Log10(rn))
			default:
				if isE {
					result = slip.DoubleFloat(math.Log(rn))
				} else {
					result = slip.DoubleFloat(math.Log2(rn) / math.Log2(rb))
				}
			}
		case slip.Complex:
			result = slip.Complex(cmplx.Log(complex(rn, 0.0)) / cmplx.Log(complex128(base)))
		default:
			slip.TypePanic(s, depth, "base", base, "number")
		}
	case slip.Complex:
		switch base := baseArg.(type) {
		case slip.Real:
			result = slip.Complex(cmplx.Log(complex128(num)) / cmplx.Log(complex(base.RealValue(), 0.0)))
		case slip.Complex:
			result = slip.Complex(cmplx.Log(complex128(num)) / cmplx.Log(complex128(base)))
		default:
			slip.TypePanic(s, depth, "base", base, "number")
		}
	default:
		slip.TypePanic(s, depth, "number", num, "number")
	}
	return
}
