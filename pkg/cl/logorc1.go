// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logorc1{Function: slip.Function{Name: "logorc1", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logorc1",
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
			Text:   `__logorc1__ returns a bit-wise OR of the _integer-2_ and the complement of _integer-1_.`,
			Examples: []string{
				"(logorc1 3 7) => -5",
			},
		}, &slip.CLPkg)
}

// Logorc1 represents the logorc1 function.
type Logorc1 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logorc1) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	return logorc1(args[0], args[1])
}

func logorc1(a1, a2 slip.Object) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = slip.Fixnum(^uint64(t1) | uint64(t2))
		case *slip.Bignum:
			result = bigLogorc1(big.NewInt(int64(t1)), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLogorc1((*big.Int)(t1), big.NewInt(int64(t2)))
		case *slip.Bignum:
			result = bigLogorc1((*big.Int)(t1), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	default:
		slip.PanicType("integer-1", t1, "integer")
	}
	return
}

func bigLogorc1(b1, b2 *big.Int) slip.Object {
	var bi big.Int
	c1 := complement((*slip.Bignum)(b1)).(*slip.Bignum)
	bi.Or((*big.Int)(c1), b2)

	return (*slip.Bignum)(&bi)
}
