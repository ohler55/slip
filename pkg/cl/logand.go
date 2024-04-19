// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logand{Function: slip.Function{Name: "logand", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logand",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "integers",
					Type: "integer",
					Text: "The values to form a bit-wise AND of.",
				},
			},
			Return: "integer",
			Text:   `__logand__ returns a bit-wise AND of all the _integers_.`,
			Examples: []string{
				"(logand 4 2 1) => 7",
			},
		}, &slip.CLPkg)
}

// Logand represents the logand function.
type Logand struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logand) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return logand(args)
}

func logand(args slip.List) slip.Object {
	var result uint64 = 0xffffffffffffffff
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			result &= uint64(tv)
		case *slip.Bignum:
			return bigLogAnd(args)
		default:
			slip.PanicType("integer", tv, "integer")
		}
	}
	return slip.Fixnum(result)
}

func bigLogAnd(args slip.List) slip.Object {
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
			if len(bb) < len(buf) {
				buf = buf[:len(bb)]
			}
			for i := len(buf) - 1; 0 <= i; i-- {
				buf[i] &= bb[i]
			}
		}
	}
	reverseBytes(buf)
	var bi big.Int
	bi.SetBytes(buf)

	return (*slip.Bignum)(&bi)
}
