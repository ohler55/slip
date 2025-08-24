// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Lognor{Function: slip.Function{Name: "lognor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lognor",
			Args: []*slip.DocArg{
				{
					Name: "integer-1",
					Type: "integer",
					Text: "An integer.",
				},
				{
					Name: "integer-2",
					Type: "integer",
					Text: "An integer.",
				},
			},
			Return: "integer",
			Text:   `__lognor__ returns a bit-wise OR of the complement of _integer-1_ and _integer-2_.`,
			Examples: []string{
				"(lognor 7 3) => -8",
			},
		}, &slip.CLPkg)
}

// Lognor represents the lognor function.
type Lognor struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Lognor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	return lognor(s, args[0], args[1], depth)
}

func lognor(s *slip.Scope, a1, a2 slip.Object, depth int) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = slip.Fixnum(^(uint64(t1) | uint64(t2)))
		case *slip.Bignum:
			result = bigLognor(big.NewInt(int64(t1)), (*big.Int)(t2))
		default:
			slip.TypePanic(s, depth, "integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLognor((*big.Int)(t1), big.NewInt(int64(t2)))
		case *slip.Bignum:
			result = bigLognor((*big.Int)(t1), (*big.Int)(t2))
		default:
			slip.TypePanic(s, depth, "integer-2", t2, "integer")
		}
	default:
		slip.TypePanic(s, depth, "integer-1", t1, "integer")
	}
	return
}

func bigLognor(b1, b2 *big.Int) slip.Object {
	var bi big.Int
	bi.Or(b1, b2)
	return complement((*slip.Bignum)(&bi)).(*slip.Bignum)
}
