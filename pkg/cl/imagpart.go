// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Imagpart{Function: slip.Function{Name: "imagpart", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "imagpart",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to determine the imaginary part of.",
				},
			},
			Return: "integer",
			Text:   `__imagpart__ returns then imaginary part of _number_.`,
			Examples: []string{
				"(imagpart 3) => 3",
				"(imagpart #C(2 3)) => 3",
			},
		}, &slip.CLPkg)
}

// Imagpart represents the imagpart function.
type Imagpart struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Imagpart) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum:
		result = slip.Fixnum(0)
	case *slip.Bignum:
		result = slip.NewBignum(0)
	case slip.SingleFloat:
		result = slip.SingleFloat(0.0)
	case slip.DoubleFloat:
		result = slip.DoubleFloat(0.0)
	case *slip.LongFloat:
		result = slip.NewLongFloat(0.0)
	case *slip.Ratio:
		result = slip.NewRatio(0, 1)
	case slip.Complex:
		result = slip.DoubleFloat(imag(ta))
	default:
		slip.TypePanic(s, depth, "number", args[0], "number")
	}
	return
}
