// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Gte{Function: slip.Function{Name: ">=", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: ">=",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "numbers",
					Type: "real",
					Text: "The numbers to compare.",
				},
			},
			Return: "boolean",
			Text: `__>=__ returns _t_ if the _numbers_ are in monotomically non-increasing order
otherwise _nil_ is returned.`,
			Examples: []string{
				"(>= 5) => t",
				"(>= 1/2 0.4) => t",
				"(>= 1 1) => t",
				"(>= 1 2) => nil",
			},
		}, &slip.CLPkg)
}

// Gte represents the != function.
type Gte struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Gte) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	if len(args) == 1 {
		if _, ok := args[0].(slip.Real); !ok {
			slip.TypePanic(s, depth, "numbers", args[0], "real")
		}
		return slip.True
	}
	var arg slip.Object
	pos := 0
	target := args[pos]
	if _, ok := target.(slip.Real); !ok {
		slip.TypePanic(s, depth, "numbers", target, "real")
	}
	pos++
	for ; pos < len(args); pos++ {
		arg, target = slip.NormalizeNumber(args[pos], target)
		switch ta := arg.(type) {
		case slip.Fixnum:
			if target.(slip.Fixnum) < ta {
				return nil
			}
		case slip.SingleFloat:
			if target.(slip.SingleFloat) < ta {
				return nil
			}
		case slip.DoubleFloat:
			if target.(slip.DoubleFloat) < ta {
				return nil
			}
		case *slip.LongFloat:
			if (*big.Float)(target.(*slip.LongFloat)).Cmp((*big.Float)(ta)) < 0 {
				return nil
			}
		case *slip.Bignum:
			if (*big.Int)(target.(*slip.Bignum)).Cmp((*big.Int)(ta)) < 0 {
				return nil
			}
		case *slip.Ratio:
			if (*big.Rat)(target.(*slip.Ratio)).Cmp((*big.Rat)(ta)) < 0 {
				return nil
			}
		case slip.Complex:
			slip.TypePanic(s, depth, "numbers", arg, "real")
		}
	}
	return slip.True
}
