// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logandc2{Function: slip.Function{Name: "logandc2", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logandc2",
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
			Text:   `__logandc2__ returns a bit-wise AND of the _integer-1_ and the complement of _integer-2_.`,
			Examples: []string{
				"(logandc2 7 3) => 4",
			},
		}, &slip.CLPkg)
}

// Logandc2 represents the logandc2 function.
type Logandc2 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logandc2) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	return logandc2(args[0], args[1])
}

func logandc2(a1, a2 slip.Object) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = slip.Fixnum(uint64(t1) & ^uint64(t2))
		case *slip.Bignum:
			result = bigLogandc1((*big.Int)(t2), big.NewInt(int64(t1)))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLogandc1(big.NewInt(int64(t2)), (*big.Int)(t1))
		case *slip.Bignum:
			result = bigLogandc1((*big.Int)(t2), (*big.Int)(t1))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	default:
		slip.PanicType("integer-1", t1, "integer")
	}
	return
}
