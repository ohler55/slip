// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RowMajorAref{Function: slip.Function{Name: "row-major-aref", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "row-major-aref",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get a value from.",
				},
				{
					Name: "index",
					Type: "fixnum",
					Text: "The major index into the array.",
				},
			},
			Return: "object",
			Text:   `__row-major-aref__ returns the indexed element of _array_.`,
			Examples: []string{
				"(row-major-aref (make-array '(2 3) :initial-contents '((a b c) (d e f))) 4) => e",
			},
		}, &slip.CLPkg)
}

// RowMajorAref represents the row-major-aref function.
type RowMajorAref struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RowMajorAref) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("index", args[1], "fixnum")
	}
	switch ta := args[0].(type) {
	case nil:
	case slip.ArrayLike:
		result = ta.MajorGet(int(index))
	default:
		slip.PanicType("array", ta, "array")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *RowMajorAref) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("index", args[1], "fixnum")
	}
	switch ta := args[0].(type) {
	case nil:
	case slip.ArrayLike:
		ta.MajorSet(int(index), value)
	default:
		slip.PanicType("array", ta, "array")
	}
}
