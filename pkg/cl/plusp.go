// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Plusp{Function: slip.Function{Name: "plusp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "plusp",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to check.",
				},
			},
			Return: "nil",
			Text:   `__plusp__ returns _true_ if _number_ is a positive number.`,
			Examples: []string{
				"(plusp 5) => t",
				"(plusp -5) => nil",
			},
		}, &slip.CLPkg)
}

// Plusp represents the plusp function.
type Plusp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Plusp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if 0 < ta {
			return slip.True
		}
	case slip.SingleFloat:
		if 0.0 < ta {
			return slip.True
		}
	case *slip.LongFloat:
		if (*big.Float)(ta).Sign() == 1 {
			return slip.True
		}
	case slip.DoubleFloat:
		if 0.0 < ta {
			return slip.True
		}
	case *slip.Bignum:
		if (*big.Int)(ta).Sign() == 1 {
			return slip.True
		}
	case *slip.Ratio:
		if 0.0 < ta.RealValue() {
			return slip.True
		}
	default:
		slip.PanicType("number", ta, "number")
	}
	return nil
}
