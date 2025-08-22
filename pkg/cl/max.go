// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Max{Function: slip.Function{Name: "max", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "max",
			Args: []*slip.DocArg{
				{
					Name: "reals",
					Type: "real",
					Text: "The numbers to find the maximum of.",
				},
			},
			Return: "real",
			Text:   `__max__ returns the maximum value of the _reals_.`,
			Examples: []string{
				"(< 5) => 5",
				"(< 1/2 0.6) => 0.6",
			},
		}, &slip.CLPkg)
}

// Max represents the != function.
type Max struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Max) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	pos := 0
	max := args[pos]
	if _, ok := max.(slip.Real); !ok {
		slip.TypePanic(s, depth, "reals", max, "real")
	}
	pos++
	for ; pos < len(args); pos++ {
		arg, mx := slip.NormalizeNumber(args[pos], max)
		switch ta := arg.(type) {
		case slip.Fixnum:
			if mx.(slip.Fixnum) < ta {
				max = args[pos]
			}
		case slip.SingleFloat:
			if mx.(slip.SingleFloat) < ta {
				max = args[pos]
			}
		case slip.DoubleFloat:
			if mx.(slip.DoubleFloat) < ta {
				max = args[pos]
			}
		case *slip.LongFloat:
			if (*big.Float)(mx.(*slip.LongFloat)).Cmp((*big.Float)(ta)) < 0 {
				max = args[pos]
			}
		case *slip.Bignum:
			if (*big.Int)(mx.(*slip.Bignum)).Cmp((*big.Int)(ta)) < 0 {
				max = args[pos]
			}
		case *slip.Ratio:
			if (*big.Rat)(mx.(*slip.Ratio)).Cmp((*big.Rat)(ta)) < 0 {
				max = args[pos]
			}
		case slip.Complex:
			slip.TypePanic(s, depth, "reals", arg, "real")
		}
	}
	return max
}
