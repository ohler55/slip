// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Lognand{Function: slip.Function{Name: "lognand", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lognand",
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
			Text:   `__lognand__ returns a bit-wise AND of the complement of _integer-1_ and _integer-2_.`,
			Examples: []string{
				"(lognand 7 3) => -4",
			},
		}, &slip.CLPkg)
}

// Lognand represents the lognand function.
type Lognand struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Lognand) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	return lognand(args[0], args[1])
}

func lognand(a1, a2 slip.Object) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = slip.Fixnum(^(uint64(t1) & uint64(t2)))
		case *slip.Bignum:
			result = bigLognand(big.NewInt(int64(t1)), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLognand((*big.Int)(t1), big.NewInt(int64(t2)))
		case *slip.Bignum:
			result = bigLognand((*big.Int)(t1), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	default:
		slip.PanicType("integer-1", t1, "integer")
	}
	return
}

func bigLognand(b1, b2 *big.Int) slip.Object {
	var bi big.Int
	bi.And(b1, b2)
	return complement((*slip.Bignum)(&bi)).(*slip.Bignum)
}
