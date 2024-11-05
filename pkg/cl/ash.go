// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Ash{Function: slip.Function{Name: "ash", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "ash",
			Args: []*slip.DocArg{
				{
					Name: "integer",
					Type: "integer",
					Text: "An integer.",
				},
				{
					Name: "shift",
					Type: "fixnum",
					Text: "The number of bit to shift the _integer_.",
				},
			},
			Return: "integer",
			Text:   `__ash__ returns _integer_ shifted _shift_ bits.`,
			Examples: []string{
				"(ash 1 2) => 4",
				"(ash 4 -2) => 1",
			},
		}, &slip.CLPkg)
}

// Ash represents the ash function.
type Ash struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Ash) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	shift, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("shift", args[1], "fixnum")
	}
	sh := int(shift)
	switch ti := args[0].(type) {
	case slip.Fixnum:
		if sh < 0 {
			result = slip.Fixnum(uint64(ti) >> -sh)
		} else {
			result = slip.Fixnum(uint64(ti) << sh)
		}
	case slip.Octet:
		if sh < 0 {
			result = slip.Octet(uint64(ti) >> -sh)
		} else {
			result = slip.Octet(uint64(ti) << sh)
		}
	case *slip.Bignum:
		ba := (*big.Int)(ti).Bytes()
		if sh < 0 {
			sh = -sh
			bs := sh / 8
			sh %= 8
			mask := byte(^(0xff << sh))
			var rem byte
			for i, b := range ba {
				ba[i] = (b >> sh) | rem
				rem = (mask & b) << (8 - sh)
			}
			ba = ba[:len(ba)-bs]
		} else {
			bs := sh / 8
			bn := make([]byte, len(ba)+bs+1)
			copy(bn[1:], ba)
			sh %= 8
			mask := byte(^(0xff >> sh))
			for i, b := range bn {
				if 0 < i {
					bn[i-1] |= (mask & b) >> (8 - sh)
				}
				bn[i] = b << sh
			}
			ba = bn
		}
		var bi big.Int
		bi.SetBytes(ba)
		if (*big.Int)(ti).Sign() < 0 {
			bi.Neg(&bi)
		}
		result = (*slip.Bignum)(&bi)
	default:
		slip.PanicType("integer", ti, "integer")
	}
	return
}
