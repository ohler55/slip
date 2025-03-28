// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayHasFillPointerP{Function: slip.Function{Name: "array-has-fill-pointer-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-has-fill-pointer-p",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to check for a fill pointer.",
				},
			},
			Return: "boolean",
			Text:   `__array-has-fill-pointer-p__ returns true if _array_ has a fill pointer.`,
			Examples: []string{
				"(array-has-fill-pointer-p (make-array 3 :fill-pointer t)) => t",
			},
		}, &slip.CLPkg)
}

// ArrayHasFillPointerP represents the array-has-fill-pointer-p function.
type ArrayHasFillPointerP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayHasFillPointerP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(slip.ArrayLike); ok {
		var v slip.FillPtrVector
		if v, ok = args[0].(slip.FillPtrVector); ok && 0 <= v.FillPointer() {
			result = slip.True
		}
	} else {
		slip.PanicType("array", args[0], "array")
	}
	return
}
