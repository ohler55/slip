// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayDimensions{Function: slip.Function{Name: "array-dimensions", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-dimensions",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the dimensions of.",
				},
			},
			Return: "list",
			Text:   `__array-dimensions__ returns the dimensions of _array_.`,
			Examples: []string{
				"(array-dimensions (make-array '(2 3)) => (2 3)",
			},
		}, &slip.CLPkg)
}

// ArrayDimensions represents the array-dimensions function.
type ArrayDimensions struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayDimensions) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	var dims []int
	switch ta := args[0].(type) {
	case *slip.Array:
		dims = ta.Dimensions()
	case *slip.Vector:
		dims = ta.Dimensions()
	default:
		slip.PanicType("array", ta, "array")
	}
	sdims := make(slip.List, len(dims))
	for i, d := range dims {
		sdims[i] = slip.Fixnum(d)
	}
	return sdims
}
