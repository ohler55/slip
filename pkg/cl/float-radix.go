// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FloatRadix{Function: slip.Function{Name: "float-radix", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "float-radix",
			Args: []*slip.DocArg{
				{
					Name: "float",
					Type: "float",
					Text: "The float to return the number of radix for.",
				},
			},
			Return: "fixnum",
			Text: `__float-radix__ returns the number of radix _b_ radix used to
represent the _float_.`,
			Examples: []string{
				"(float-radix 3.0s0) => 2",
				"(float-radix 3.0d0) => 2",
			},
		}, &slip.CLPkg)
}

// FloatRadix represents the float-radix function.
type FloatRadix struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FloatRadix) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(slip.Float); !ok {
		slip.PanicType("float", args[0], "float")
	}
	return slip.Fixnum(2)
}
