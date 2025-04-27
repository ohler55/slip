// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Min{Function: slip.Function{Name: "min", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "min",
			Args: []*slip.DocArg{
				{
					Name: "reals",
					Type: "real",
					Text: "The numbers to find the minimum of.",
				},
			},
			Return: "real",
			Text:   `__min__ returns the minimum value of the _reals_.`,
			Examples: []string{
				"(< 5) => 5",
				"(< 1/2 0.6) => 0.6",
			},
		}, &slip.CLPkg)
}

// Min represents the != function.
type Min struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Min) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) == 0 {
		slip.PanicArgCount(f, 1, -1)
	}
	pos := 0
	min := args[pos]
	if _, ok := min.(slip.Real); !ok {
		slip.PanicType("reals", min, "real")
	}
	pos++
	for ; pos < len(args); pos++ {
		arg, mx := slip.NormalizeNumber(args[pos], min)
		switch ta := arg.(type) {
		case slip.Fixnum:
			if mx.(slip.Fixnum) > ta {
				min = args[pos]
			}
		case slip.SingleFloat:
			if mx.(slip.SingleFloat) > ta {
				min = args[pos]
			}
		case slip.DoubleFloat:
			if mx.(slip.DoubleFloat) > ta {
				min = args[pos]
			}
		case *slip.LongFloat:
			if (*big.Float)(mx.(*slip.LongFloat)).Cmp((*big.Float)(ta)) > 0 {
				min = args[pos]
			}
		case *slip.Bignum:
			if (*big.Int)(mx.(*slip.Bignum)).Cmp((*big.Int)(ta)) > 0 {
				min = args[pos]
			}
		case *slip.Ratio:
			if (*big.Rat)(mx.(*slip.Ratio)).Cmp((*big.Rat)(ta)) > 0 {
				min = args[pos]
			}
		case slip.Complex:
			slip.PanicType("reals", arg, "real")
		}
	}
	return min
}
