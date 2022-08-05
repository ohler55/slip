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

// Call the the function with the arguments provided.
func (f *Same) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 {
		slip.PanicArgCount(f, 1, -1)
	}
	var target slip.Object
	var arg slip.Object
	for pos := len(args) - 1; 0 <= pos; pos-- {
		if target == nil {
			target = args[pos]
			if _, ok := target.(slip.Number); !ok {
				slip.PanicType("numbers", target, "number")
			}
			continue
		}
		arg, target = normalizeNumber(args[pos], target)
		switch ta := arg.(type) {
		case slip.Fixnum:
			if target.(slip.Fixnum) != ta {
				return nil
			}
		case slip.SingleFloat:
			if target.(slip.SingleFloat) != ta {
				return nil
			}
		case slip.DoubleFloat:
			if target.(slip.DoubleFloat) != ta {
				return nil
			}
		case *slip.LongFloat:
			if (*big.Float)(target.(*slip.LongFloat)).Cmp((*big.Float)(ta)) != 0 {
				return nil
			}
		case *slip.Bignum:
			if (*big.Int)(target.(*slip.Bignum)).Cmp((*big.Int)(ta)) != 0 {
				return nil
			}
		case *slip.Ratio:
			if (*big.Rat)(target.(*slip.Ratio)).Cmp((*big.Rat)(ta)) != 0 {
				return nil
			}
		case slip.Complex:
			if complex128(target.(slip.Complex)) != complex128(ta) {
				return nil
			}
		}
	}
	return slip.True
}
