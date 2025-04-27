// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayTotalSize{Function: slip.Function{Name: "array-total-size", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-total-size",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the total size of.",
				},
			},
			Return: "fixnum",
			Text:   `__array-total-size__ returns the total size of _array_.`,
			Examples: []string{
				"(array-total-size (make-array '(2 3)) => 2",
			},
		}, &slip.CLPkg)
}

// ArrayTotalSize represents the array-total-size function.
type ArrayTotalSize struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayTotalSize) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if al, ok := args[0].(slip.ArrayLike); ok {
		result = slip.Fixnum(al.Length())
	} else {
		slip.PanicType("array", args[0], "array")
	}
	return
}
