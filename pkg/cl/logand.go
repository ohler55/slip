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
	var bi big.Int
	first := true
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			if first {
				_ = bi.SetInt64(int64(tv))
				first = false
			} else {
				_ = bi.And(&bi, big.NewInt(int64(tv)))
			}
		case *slip.Bignum:
			if first {
				bi.Set((*big.Int)(tv))
				first = false
			} else {
				_ = bi.And(&bi, (*big.Int)(tv))
			}
		default:
			slip.PanicType("integer", tv, "integer")
		}
	}
	return (*slip.Bignum)(&bi)
}
