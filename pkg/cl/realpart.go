// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Realpart{Function: slip.Function{Name: "realpart", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "realpart",
			Args: []*slip.DocArg{
				{
					Name: "number",
					Type: "number",
					Text: "The number to determine the real part of.",
				},
			},
			Return: "integer",
			Text:   `__realpart__ returns then real part of _number_.`,
			Examples: []string{
				"(realpart 3) => 3",
				"(realpart #C(2 3)) => 2",
			},
		}, &slip.CLPkg)
}

// Realpart represents the realpart function.
type Realpart struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Realpart) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.Fixnum, *slip.Bignum, slip.SingleFloat, slip.DoubleFloat, *slip.LongFloat, *slip.Ratio:
		result = ta
	case slip.Complex:
		result = slip.DoubleFloat(real(ta))
	default:
		slip.TypePanic(s, depth, "number", args[0], "number")
	}
	return
}
