// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logorc2{Function: slip.Function{Name: "logorc2", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logorc2",
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
			Text:   `__logorc2__ returns a bit-wise OR of the _integer-2_ and the complement of _integer-1_.`,
			Examples: []string{
				"(logorc2 7 3) => -5",
			},
		}, &slip.CLPkg)
}

// Logorc2 represents the logorc2 function.
type Logorc2 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logorc2) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	return logorc2(s, args[0], args[1], depth)
}

func logorc2(s *slip.Scope, a1, a2 slip.Object, depth int) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = slip.Fixnum(uint64(t1) | ^uint64(t2))
		case *slip.Bignum:
			result = bigLogorc1((*big.Int)(t2), big.NewInt(int64(t1)))
		default:
			slip.TypePanic(s, depth, "integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLogorc1(big.NewInt(int64(t2)), (*big.Int)(t1))
		case *slip.Bignum:
			result = bigLogorc1((*big.Int)(t2), (*big.Int)(t1))
		default:
			slip.TypePanic(s, depth, "integer-2", t2, "integer")
		}
	default:
		slip.TypePanic(s, depth, "integer-1", t1, "integer")
	}
	return
}
