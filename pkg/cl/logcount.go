// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Logcount{Function: slip.Function{Name: "logcount", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "logcount",
			Args: []*slip.DocArg{
				{
					Name: "integer",
					Type: "integer",
					Text: "An integer.",
				},
			},
			Return: "boolean",
			Text: `__logcount__ returns the number of set bits in _integer_ if _integer_
is a positive value and the number of unset bits if _integer_ is negative.`,
			Examples: []string{
				"(logcount 5) => 2",
				"(logcount #7fff) => 15",
			},
		}, &slip.CLPkg)
}

// Logcount represents the logcount function.
type Logcount struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Logcount) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var cnt slip.Fixnum
	switch ti := args[0].(type) {
	case slip.Fixnum:
		u := uint64(ti)
		if 0 <= ti {
			for i := 0; i < 64; i++ {
				if (u>>i)&0x01 == 1 {
					cnt++
				}
			}
		} else {
			for i := 0; i < 64; i++ {
				if (u>>i)&0x01 != 1 {
					cnt++
				}
			}
		}
	case *slip.Bignum:
		ba := (*big.Int)(ti).Bytes()
		for _, b := range ba {
			for i := 0; i < 8; i++ {
				if (b>>i)&0x01 == 1 {
					cnt++
				}
			}
		}
		if (*big.Int)(ti).Sign() < 0 && 0 < cnt {
			cnt--
		}
	default:
		slip.TypePanic(s, depth, "integer", ti, "integer")
	}
	return cnt
}
