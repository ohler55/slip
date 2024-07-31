// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayDisplacement{Function: slip.Function{Name: "array-displacement", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-displacement",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the displacement of.",
				},
			},
			Return: "array,fixnum",
			Text:   `__array-displacement__ returns the displacement of _array_.`,
			Examples: []string{
				"(array-displacement (make-array '(2 3)) => nil, 0",
			},
		}, &slip.CLPkg)
}

// ArrayDisplacement represents the array-displacement function.
type ArrayDisplacement struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayDisplacement) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.Array, *slip.Vector, slip.Octets:
		result = slip.Values{nil, slip.Fixnum(0)}
	default:
		slip.PanicType("array", ta, "array")
	}
	return
}
