// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Evenp{Function: slip.Function{Name: "evenp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "evenp",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "integer",
					Text: "The integer to check.",
				},
			},
			Return: "nil",
			Text:   `__evenp__ returns _true_ if _number_ is an even integer.`,
			Examples: []string{
				"(evenp 4) => t",
				"(evenp 5) => nil",
			},
		}, &slip.CLPkg)
}

// Evenp represents the evenp function.
type Evenp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Evenp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta%2 == 0 {
			return slip.True
		}
	case slip.Octet:
		if ta%2 == 0 {
			return slip.True
		}
	case *slip.Bignum:
		var bi big.Int
		_ = bi.Mod((*big.Int)(ta), big.NewInt(2))
		if bi.Int64() == 0 {
			return slip.True
		}
	default:
		slip.PanicType("number", ta, "integer")
	}
	return nil
}
