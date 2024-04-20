// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Lognot{Function: slip.Function{Name: "lognot", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lognot",
			Args: []*slip.DocArg{
				{
					Name: "integer",
					Type: "integer",
					Text: "The value to form a bit-wise NOT from.",
				},
			},
			Return: "integer",
			Text:   `__lognot__ returns a bit-wise NOT of the _integer_.`,
			Examples: []string{
				"(lognot 7) => -8",
			},
		}, &slip.CLPkg)
}

// Lognot represents the lognot function.
type Lognot struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Lognot) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	return lognot(args[0])
}

func lognot(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Fixnum:
		result = slip.Fixnum(^uint64(ta))
	case *slip.Bignum:
		var bi big.Int
		_ = bi.Not((*big.Int)(ta))
		result = (*slip.Bignum)(&bi)
	default:
		slip.PanicType("integer", ta, "integer")
	}
	return
}
