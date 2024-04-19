// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logxor{Function: slip.Function{Name: "logxor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logxor",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "integers",
					Type: "integer",
					Text: "The values to form a bit-wise XOR of.",
				},
			},
			Return: "integer",
			Text:   `__logxor__ returns a bit-wise XOR of all the _integers_.`,
			Examples: []string{
				"(logxor 4 7 1) => 2",
			},
		}, &slip.CLPkg)
}

// Logxor represents the logxor function.
type Logxor struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logxor) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return logxor(args)
}

func logxor(args slip.List) slip.Object {
	var result uint64
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			result ^= uint64(tv)
		case *slip.Bignum:
			return bigLogxor(args)
		default:
			slip.PanicType("integer", tv, "integer")
		}
	}
	return slip.Fixnum(result)
}

func bigLogxor(args slip.List) slip.Object {
	var buf []byte // bytes in reverse
	for _, v := range args {
		var bb []byte
		switch tv := v.(type) {
		case slip.Fixnum:
			bb = fixnumRevBytes(tv)
		case *slip.Bignum:
			bb = (*big.Int)(tv).Bytes()
			reverseBytes(bb)
		default:
			slip.PanicType("integer", tv, "integer")
		}
		if len(buf) == 0 {
			buf = bb
		} else {
			if len(bb) <= len(buf) {
				for i, b := range bb {
					buf[i] ^= b
				}
			} else {
				for i, b := range buf {
					bb[i] ^= b
				}
				buf = bb
			}
		}
	}
	reverseBytes(buf)
	var bi big.Int
	bi.SetBytes(buf)

	return (*slip.Bignum)(&bi)
}
