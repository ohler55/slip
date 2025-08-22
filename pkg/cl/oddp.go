// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Oddp{Function: slip.Function{Name: "oddp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "oddp",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "integer",
					Text: "The integer to check.",
				},
			},
			Return: "nil",
			Text:   `__oddp__ returns _true_ if _number_ is an odd integer.`,
			Examples: []string{
				"(oddp 4) => t",
				"(oddp 5) => nil",
			},
		}, &slip.CLPkg)
}

// Oddp represents the oddp function.
type Oddp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Oddp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta%2 == 1 {
			return slip.True
		}
	case slip.Octet:
		if ta%2 == 1 {
			return slip.True
		}
	case *slip.Bignum:
		var bi big.Int
		_ = bi.Mod((*big.Int)(ta), big.NewInt(2))
		if bi.Int64() == 1 {
			return slip.True
		}
	default:
		slip.TypePanic(s, depth, "number", ta, "integer")
	}
	return nil
}
