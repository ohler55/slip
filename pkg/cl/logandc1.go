// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logandc1{Function: slip.Function{Name: "logandc1", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logandc1",
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
			Text:   `__logandc1__ returns a bit-wise AND of the _integer-2_ and the complement of _integer-1_.`,
			Examples: []string{
				"(logandc1 3 7) => 4",
			},
		}, &slip.CLPkg)
}

// Logandc1 represents the logandc1 function.
type Logandc1 struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logandc1) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	return logandc1(args[0], args[1])
}

func logandc1(a1, a2 slip.Object) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = slip.Fixnum(^uint64(t1) & uint64(t2))
		case *slip.Bignum:
			result = bigLogandc1(big.NewInt(int64(t1)), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLogandc1((*big.Int)(t1), big.NewInt(int64(t2)))
		case *slip.Bignum:
			result = bigLogandc1((*big.Int)(t1), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	default:
		slip.PanicType("integer-1", t1, "integer")
	}
	return
}

func bigLogandc1(b1, b2 *big.Int) slip.Object {
	bb1 := b1.Bytes()
	reverseBytes(bb1)
	bb2 := b2.Bytes()
	reverseBytes(bb2)

	var (
		i int
		b byte
	)
	if len(bb1) <= len(bb2) {
		for i, b = range bb1 {
			bb2[i] &= ^b
		}
		for i++; i < len(bb2); i++ {
			bb2[i] = bb2[i] & 0xff
		}
	} else {
		for i, _ = range bb2 {
			bb2[i] &= ^bb1[i]
		}
	}
	reverseBytes(bb2)
	var bi big.Int
	bi.SetBytes(bb2)

	return (*slip.Bignum)(&bi)
}
