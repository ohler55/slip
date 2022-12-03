// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Zerop{Function: slip.Function{Name: "zerop", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "zerop",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to check.",
				},
			},
			Return: "nil",
			Text:   `__zerop__ returns _true_ if _number_ is zero.`,
			Examples: []string{
				"(zerop 0) => t",
				"(zerop 5) => nil",
			},
		}, &slip.CLPkg)
}

// Zerop represents the zerop function.
type Zerop struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Zerop) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta == 0 {
			return slip.True
		}
	case slip.SingleFloat:
		if ta == 0.0 {
			return slip.True
		}
	case *slip.LongFloat:
		if (*big.Float)(ta).Sign() == 0 {
			return slip.True
		}
	case slip.DoubleFloat:
		if ta == 0.0 {
			return slip.True
		}
	case *slip.Bignum:
		if (*big.Int)(ta).Sign() == 0 {
			return slip.True
		}
	case *slip.Ratio:
		if ta.RealValue() == 0.0 {
			return slip.True
		}
	default:
		slip.PanicType("number", ta, "number")
	}
	return nil
}
