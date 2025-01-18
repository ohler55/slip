// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Uniq{Function: slip.Function{Name: "/=", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "/=",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "numbers",
					Type: "number",
					Text: "The numbers to check for not being the same value.",
				},
			},
			Return: "boolean",
			Text: `__/=__ returns _t_ if all the _numbers_ have different values or
_nil_ if any two have the same value.`,
			Examples: []string{
				"(/= 5) => t",
				"(/= 1/2 0.5) => nil",
				"(/= 1 2) => t",
			},
		}, &slip.CLPkg)
}

// Uniq represents the != function.
type Uniq struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Uniq) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	switch len(args) {
	case 0:
		slip.PanicArgCount(f, 1, -1)
	case 1:
		if _, ok := args[0].(slip.Number); !ok {
			slip.PanicType("numbers", args[0], "number")
		}
		return slip.True
	}
	// Maybe not the most efficient approach. For each number walk the rest of
	// the list looking for a match. Keep shortening the list until and repeat
	// until only one remains.
	var arg slip.Object
	for start := len(args) - 1; 0 < start; start-- {
		var target slip.Object
		for pos := start; 0 <= pos; pos-- {
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
				if target.(slip.Fixnum) == ta {
					return nil
				}
			case slip.SingleFloat:
				if target.(slip.SingleFloat) == ta {
					return nil
				}
			case slip.DoubleFloat:
				if target.(slip.DoubleFloat) == ta {
					return nil
				}
			case *slip.LongFloat:
				if (*big.Float)(target.(*slip.LongFloat)).Cmp((*big.Float)(ta)) == 0 {
					return nil
				}
			case *slip.Bignum:
				if (*big.Int)(target.(*slip.Bignum)).Cmp((*big.Int)(ta)) == 0 {
					return nil
				}
			case *slip.Ratio:
				if (*big.Rat)(target.(*slip.Ratio)).Cmp((*big.Rat)(ta)) == 0 {
					return nil
				}
			case slip.Complex:
				if complex128(target.(slip.Complex)) == complex128(ta) {
					return nil
				}
			}
		}
	}
	return slip.True
}
