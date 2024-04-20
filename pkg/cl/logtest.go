// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logtest{Function: slip.Function{Name: "logtest", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logtest",
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
			Text:   `__logtest__ returns true if and of the bits set in _integer-1_ are also set in _integer-2_.`,
			Examples: []string{
				"(logtest 7 3) => -4",
			},
		}, &slip.CLPkg)
}

// Logtest represents the logtest function.
type Logtest struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logtest) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	return logtest(args[0], args[1])
}

func logtest(a1, a2 slip.Object) (result slip.Object) {
	switch t1 := a1.(type) {
	case slip.Fixnum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			if uint64(t1)&uint64(t2) != 0 {
				result = slip.True
			}
		case *slip.Bignum:
			result = bigLogtest(big.NewInt(int64(t1)), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	case *slip.Bignum:
		switch t2 := a2.(type) {
		case slip.Fixnum:
			result = bigLogtest((*big.Int)(t1), big.NewInt(int64(t2)))
		case *slip.Bignum:
			result = bigLogtest((*big.Int)(t1), (*big.Int)(t2))
		default:
			slip.PanicType("integer-2", t2, "integer")
		}
	default:
		slip.PanicType("integer-1", t1, "integer")
	}
	return
}

func bigLogtest(b1, b2 *big.Int) slip.Object {
	bb1 := b1.Bytes()
	reverseBytes(bb1)
	bb2 := b2.Bytes()
	reverseBytes(bb2)
	// Reversed so the least significant for both are at the start.
	if len(bb1) <= len(bb2) {
		for i, b := range bb1 {
			if bb2[i]&b != 0 {
				return slip.True
			}
		}
	} else {
		for i, b := range bb2 {
			if bb1[i]&b != 0 {
				return slip.True
			}
		}
	}
	return nil
}
