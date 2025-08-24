// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := IntegerLength{Function: slip.Function{Name: "integer-length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "integer-length",
			Args: []*slip.DocArg{
				{Name: "integer", Type: "integer"},
			},
			Return: "fixnum",
			Text:   `__integer-length__ returns the number of bits needed to represent _integer_.`,
			Examples: []string{
				"(integer-length) => 0",
				"(integer-length 3) => 3",
				"(integer-length 70 42) => 7",
			},
		}, &slip.CLPkg)
}

// IntegerLength represents the integer-length function.
type IntegerLength struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *IntegerLength) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta < 0 {
			ta = -(ta + 1)
		}
		var i int
		for i = 0; i < 64; i++ {
			if ta>>i == 0 {
				break
			}
		}
		result = slip.Fixnum(i)
	case *slip.Bignum:
		bi := (*big.Int)(ta)
		if bi.Sign() < 0 {
			bi = bi.Add(bi, big.NewInt(1))
			bi = bi.Neg(bi)
		}
		result = slip.Fixnum(bi.BitLen())
	default:
		slip.TypePanic(s, depth, "integer", ta, "integer")
	}
	return
}
