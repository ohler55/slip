// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayRowMajorIndex{Function: slip.Function{Name: "array-row-major-index", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-row-major-index",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to calculate the offset for.",
				},
				{Name: "&rest"},
				{
					Name: "subscripts",
					Type: "fixnum",
					Text: "The indices into the array.",
				},
			},
			Return: "object",
			Text: `__array-row-major-index__ returns the position ordering in the _array_
indexed by the _subscripts_.`,
			Examples: []string{
				"(array-row-major-index (make-array '(2 3) 1 2) => 5",
			},
		}, &slip.CLPkg)
}

// ArrayRowMajorIndex represents the array-row-major-index function.
type ArrayRowMajorIndex struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayRowMajorIndex) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	indices := make([]int, 0, len(args)-1)
	for _, a := range args[1:] {
		if num, ok := a.(slip.Fixnum); ok {
			indices = append(indices, int(num))
		} else {
			slip.TypePanic(s, depth, "subscript", a, "fixnum")
		}
	}
	if al, ok := args[0].(slip.ArrayLike); ok {
		result = slip.Fixnum(al.MajorIndex(indices...))
	} else {
		slip.TypePanic(s, depth, "array", args[0], "array")
	}
	return
}
