// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Aref{Function: slip.Function{Name: "aref", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "aref",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get a value from.",
				},
				{Name: "&rest"},
				{
					Name: "subscripts",
					Type: "fixnum",
					Text: "The indices into the array.",
				},
			},
			Return: "object",
			Text:   `__aref__ returns the indexed element of _array_.`,
			Examples: []string{
				"(aref (make-array '(2 3) :initial-contents '((a b c) (d e f))) 0 2) => c",
			},
		}, &slip.CLPkg)
}

// Aref represents the aref function.
type Aref struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Aref) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, -1)
	indices := make([]int, 0, len(args)-1)
	for _, a := range args[1:] {
		if num, ok := a.(slip.Fixnum); ok {
			indices = append(indices, int(num))
		} else {
			slip.TypePanic(s, depth, "subscript", a, "fixnum")
		}
	}
	al, ok := args[0].(slip.ArrayLike)
	if !ok {
		slip.TypePanic(s, depth, "array", args[0], "array")
	}
	return al.Get(indices...)
}

// Place a value in the first position of a list or cons.
func (f *Aref) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 1, -1)
	indices := make([]int, 0, len(args)-1)
	for _, a := range args[1:] {
		if num, ok := a.(slip.Fixnum); ok {
			indices = append(indices, int(num))
		} else {
			slip.TypePanic(s, 0, "subscript", a, "fixnum")
		}
	}
	al, ok := args[0].(slip.ArrayLike)
	if !ok {
		slip.TypePanic(s, 0, "array", args[0], "array")
	}
	al.Set(value, indices...)
}
