// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayElementType{Function: slip.Function{Name: "array-element-type", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-element-type",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the element type of.",
				},
			},
			Return: "symbol",
			Text:   `__array-element-type__ returns the element type of _array_.`,
			Examples: []string{
				"(array-element-type (make-array '(2 3) :element-type 'fixnum) => fixnum",
			},
		}, &slip.CLPkg)
}

// ArrayElementType represents the array-element-type function.
type ArrayElementType struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayElementType) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if al, ok := args[0].(slip.ArrayLike); ok {
		result = al.ElementType()
	} else {
		slip.PanicType("array", args[0], "array")
	}
	return
}
