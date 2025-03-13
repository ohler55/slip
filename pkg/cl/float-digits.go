// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FloatDigits{Function: slip.Function{Name: "float-digits", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "float-digits",
			Args: []*slip.DocArg{
				{
					Name: "float",
					Type: "float",
					Text: "The float to return the number of digits for.",
				},
			},
			Return: "fixnum",
			Text: `__float-digits__ returns the number of radix _b_ digits used to
represent the _float_.`,
			Examples: []string{
				"(float-digits 3.0s0) => 24",
				"(float-digits 3.0d0) => 53",
				"(float-digits 5.1234L0) => 19",
			},
		}, &slip.CLPkg)
}

// FloatDigits represents the float-digits function.
type FloatDigits struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FloatDigits) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
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
