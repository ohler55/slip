// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayDimension{Function: slip.Function{Name: "array-dimension", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-dimension",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the dimension of.",
				},
				{
					Name: "axis-number",
					Type: "fixnum",
					Text: "The axis to get the dimension of.",
				},
			},
			Return: "fixnum",
			Text:   `__array-dimension__ returns the dimension of _axis-number_ of _array_.`,
			Examples: []string{
				"(array-dimension (make-array '(2 3)) 1) => 3",
			},
		}, &slip.CLPkg)
}

// ArrayDimension represents the array-dimension function.
type ArrayDimension struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayDimension) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	var (
		axis int
		dims []int
	)
	if al, ok := args[0].(slip.ArrayLike); ok {
		dims = al.Dimensions()
	} else {
		slip.TypePanic(s, depth, "array", args[0], "array")
	}
	if num, ok := args[1].(slip.Fixnum); ok {
		axis = int(num)
	} else {
		slip.TypePanic(s, depth, "axis-number", args[1], "fixnum")
	}
	if axis < 0 || len(dims) <= axis {
		slip.ErrorPanic(s, depth, "Axis number %d is out of range for array of rank %d.", axis, len(dims))
	}
	return slip.Fixnum(dims[axis])
}
