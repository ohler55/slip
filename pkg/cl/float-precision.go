// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FloatPrecision{Function: slip.Function{Name: "float-precision", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "float-precision",
			Args: []*slip.DocArg{
				{
					Name: "float",
					Type: "float",
					Text: "The float to return the number of precision for.",
				},
			},
			Return: "fixnum",
			Text: `__float-precision__ returns the number of radix _b_ precision used to
represent the _float_.`,
			Examples: []string{
				"(float-precision 3.0s0) => 24",
				"(float-precision 3.0d0) => 53",
				"(float-precision 5.1234L0) => 19",
			},
		}, &slip.CLPkg)
}

// FloatPrecision represents the float-precision function.
type FloatPrecision struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FloatPrecision) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case slip.DoubleFloat:
		result = slip.Fixnum(53)
	case slip.SingleFloat:
		result = slip.Fixnum(24)
	case *slip.LongFloat:
		result = slip.Fixnum((*big.Float)(ta).Prec())
	default:
		slip.PanicType("float", args[0], "float")
	}
	return
}
