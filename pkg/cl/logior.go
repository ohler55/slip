// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logior{Function: slip.Function{Name: "logior", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logior",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "integers",
					Type: "integer",
					Text: "The values to form a bit-wise OR of.",
				},
			},
			Return: "integer",
			Text:   `__logior__ returns a bit-wise OR of all the _integers_.`,
			Examples: []string{
				"(logior 4 2 1) => 7",
			},
		}, &slip.CLPkg)
}

// Logior represents the logior function.
type Logior struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logior) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return logior(args)
}

func logior(args slip.List) slip.Object {
	var result uint64
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			result |= uint64(tv)
		case *slip.Bignum:
			return bigLogior(args)
		default:
			slip.PanicType("integer", tv, "integer")
		}
	}
	return slip.Fixnum(result)
}

func bigLogior(args slip.List) slip.Object {
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
					buf[i] |= b
				}
			} else {
				for i, b := range buf {
					bb[i] |= b
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
