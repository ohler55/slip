// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayInBoundsP{Function: slip.Function{Name: "array-in-bounds-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-in-bounds-p",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to check for being in bounds.",
				},
				{Name: "&rest"},
				{
					Name: "subscripts",
					Type: "fixnum",
					Text: "The indices into the array.",
				},
			},
			Return: "boolean",
			Text:   `__array-in-bounds-p__ returns true if _subscripts_ refer to a location in the _array_.`,
			Examples: []string{
				"(array-in-bounds-p (make-array 3) 1) => t",
			},
		}, &slip.CLPkg)
}

// ArrayInBoundsP represents the array-in-bounds-p function.
type ArrayInBoundsP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayInBoundsP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	var dims []int
	if al, ok := args[0].(slip.ArrayLike); ok {
		dims = al.Dimensions()
	} else {
		slip.PanicType("array", args[0], "array")
	}
	if len(dims) != len(args)-1 {
		slip.NewPanic("Wrong number of subscripts, %d, for array of rank %d.", len(args)-1, len(dims))
	}
	for i, d := range dims {
		if num, ok := args[i+1].(slip.Fixnum); ok && 0 <= num {
			if d <= int(num) {
				return nil
			}
		} else {
			slip.PanicType("subscript", args[i+1], "list of fixnum")
		}
	}
	return slip.True
}
