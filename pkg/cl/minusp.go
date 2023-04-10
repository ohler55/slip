// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Minusp{Function: slip.Function{Name: "minusp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "minusp",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to check.",
				},
			},
			Return: "nil",
			Text:   `__minusp__ returns _true_ if _number_ is a negative number.`,
			Examples: []string{
				"(minusp -5) => t",
				"(minusp 5) => nil",
			},
		}, &slip.CLPkg)
}

// Minusp represents the minusp function.
type Minusp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Minusp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta < 0 {
			return slip.True
		}
	case slip.SingleFloat:
		if ta < 0.0 {
			return slip.True
		}
	case *slip.LongFloat:
		if (*big.Float)(ta).Sign() == -1 {
			return slip.True
		}
	case slip.DoubleFloat:
		if ta < 0.0 {
			return slip.True
		}
	case *slip.Bignum:
		if (*big.Int)(ta).Sign() == -1 {
			return slip.True
		}
	case *slip.Ratio:
		if ta.RealValue() < 0.0 {
			return slip.True
		}
	default:
		slip.PanicType("number", ta, "number")
	}
	return nil
}
