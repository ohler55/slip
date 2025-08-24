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
	return logxor(s, args, depth)
}

func logxor(s *slip.Scope, args slip.List, depth int) slip.Object {
	var result uint64
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			result ^= uint64(tv)
		case *slip.Bignum:
			return bigLogxor(s, args, depth)
		default:
			slip.TypePanic(s, depth, "integer", tv, "integer")
		}
	}
	return slip.Fixnum(result)
}

func bigLogxor(s *slip.Scope, args slip.List, depth int) slip.Object {
	var bi big.Int
	first := true
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			if first {
				_ = bi.SetInt64(int64(tv))
				first = false
			} else {
				_ = bi.Xor(&bi, big.NewInt(int64(tv)))
			}
		case *slip.Bignum:
			if first {
				bi.Set((*big.Int)(tv))
				first = false
			} else {
				_ = bi.Xor(&bi, (*big.Int)(tv))
			}
		default:
			slip.TypePanic(s, depth, "integer", tv, "integer")
		}
	}
	return (*slip.Bignum)(&bi)
}
