// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Denominator{Function: slip.Function{Name: "denominator", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "denominator",
			Args: []*slip.DocArg{
				{
					Name: "rational",
					Type: "rational",
					Text: "The rational number to determine the denominator of.",
				},
			},
			Return: "integer",
			Text:   `__denominator__ returns then demoninator of _rational_.`,
			Examples: []string{
				"(denominator 3) => 1",
				"(denominator 3/4) => 4",
			},
		}, &slip.CLPkg)
}

// Denominator represents the denominator function.
type Denominator struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Denominator) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum, *slip.Bignum:
		result = slip.Fixnum(1)
	case *slip.Ratio:
		bi := (*big.Rat)(ta).Denom()
		if bi.IsInt64() {
			result = slip.Fixnum(bi.Int64())
		} else {
			result = (*slip.Bignum)(bi)
		}
	default:
		slip.PanicType("rational", args[0], "rational")
	}
	return
}
