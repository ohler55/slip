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
	return logior(s, args, depth)
}

func logior(s *slip.Scope, args slip.List, depth int) slip.Object {
	var result uint64
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			result |= uint64(tv)
		case *slip.Bignum:
			return bigLogior(s, args, depth)
		default:
			slip.TypePanic(s, depth, "integer", tv, "integer")
		}
	}
	return slip.Fixnum(result)
}

func bigLogior(s *slip.Scope, args slip.List, depth int) slip.Object {
	var bi big.Int
	first := true
	for _, v := range args {
		switch tv := v.(type) {
		case slip.Fixnum:
			if first {
				_ = bi.SetInt64(int64(tv))
				first = false
			} else {
				_ = bi.Or(&bi, big.NewInt(int64(tv)))
			}
		case *slip.Bignum:
			if first {
				bi.Set((*big.Int)(tv))
				first = false
			} else {
				_ = bi.Or(&bi, (*big.Int)(tv))
			}
		default:
			slip.TypePanic(s, depth, "integer", tv, "integer")
		}
	}
	return (*slip.Bignum)(&bi)
}
