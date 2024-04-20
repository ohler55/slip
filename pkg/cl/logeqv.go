// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logeqv{Function: slip.Function{Name: "logeqv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logeqv",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "integers",
					Type: "integer",
					Text: "The values to form a bit-wise XNOR of.",
				},
			},
			Return: "integer",
			Text:   `__logeqv__ returns a bit-wise XNOR of all the _integers_.`,
			Examples: []string{
				"(logeqv 7 3 1) => 5",
			},
		}, &slip.CLPkg)
}

// Logeqv represents the logeqv function.
type Logeqv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logeqv) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return logeqv(args)
}

func logeqv(args slip.List) slip.Object {
	var result uint64 = 0xffffffffffffffff
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			result ^= ^uint64(tv)
		case *slip.Bignum:
			return bigLogeqv(args)
		default:
			slip.PanicType("integer", tv, "integer")
		}
	}
	return slip.Fixnum(result)
}

func bigLogeqv(args slip.List) slip.Object {
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
			var (
				i int
				b byte
			)
			if len(bb) <= len(buf) {
				for i, b = range bb {
					buf[i] ^= ^b
				}
				for i++; i < len(buf); i++ {
					buf[i] ^= 0xff
				}
			} else {
				for i, b = range buf {
					bb[i] ^= ^b
				}
				for i++; i < len(bb); i++ {
					bb[i] ^= 0xff
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
