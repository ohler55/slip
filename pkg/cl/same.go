// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Same{Function: slip.Function{Name: "=", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "=",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "numbers",
					Type: "number",
					Text: "The number to check for the same value.",
				},
			},
			Return: "boolean",
			Text:   `__=__ returns _t_ if all the _numbers_ have the same value or _nil_ otherwise.`,
			Examples: []string{
				"(= 5) => t",
				"(= 1/2 0.5) => t",
				"(= 1 2) => nil",
			},
		}, &slip.CLPkg)
}

// Same represents the = function.
type Same struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Same) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	var target slip.Object
	for pos := len(args) - 1; 0 <= pos; pos-- {
		if target == nil {
			target = args[pos]
			if _, ok := target.(slip.Number); !ok {
				slip.TypePanic(s, depth, "numbers", target, "number")
			}
			continue
		}
		if target = same(args[pos], target); target == nil {
			return nil
		}
	}
	return slip.True
}

func same(x, y slip.Object) slip.Object {
	x, y = slip.NormalizeNumber(x, y)
	switch tx := x.(type) {
	case slip.Fixnum:
		if y.(slip.Fixnum) != tx {
			return nil
		}
	case slip.SingleFloat:
		if y.(slip.SingleFloat) != tx {
			return nil
		}
	case slip.DoubleFloat:
		if y.(slip.DoubleFloat) != tx {
			return nil
		}
	case *slip.LongFloat:
		if (*big.Float)(y.(*slip.LongFloat)).Cmp((*big.Float)(tx)) != 0 {
			return nil
		}
	case *slip.Bignum:
		if (*big.Int)(y.(*slip.Bignum)).Cmp((*big.Int)(tx)) != 0 {
			return nil
		}
	case *slip.Ratio:
		if (*big.Rat)(y.(*slip.Ratio)).Cmp((*big.Rat)(tx)) != 0 {
			return nil
		}
	case slip.Complex:
		if complex128(y.(slip.Complex)) != complex128(tx) {
			return nil
		}
	}
	return y
}
