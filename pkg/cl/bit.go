// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Bit{Function: slip.Function{Name: "bit", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bit",
			Args: []*slip.DocArg{
				{
					Name: "bit-array",
					Type: "bit-array",
					Text: "The bit-array to get a value from.",
				},
				{Name: "&rest"},
				{
					Name: "subscripts",
					Type: "fixnum",
					Text: "The indices into the array.",
				},
			},
			Return: "bit",
			Text:   `__bit__ returns the indexed bit element of _array_.`,
			Examples: []string{
				"(bit (make-array '(2 3) :element-type 'bit :initial-contents '((1 0 1) (0 1 0))) 0 2) => 1",
			},
		}, &slip.CLPkg)
}

// Bit represents the bit function.
type Bit struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Bit) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, -1)
	indices := make([]int, 0, len(args)-1)
	for _, a := range args[1:] {
		if num, ok := a.(slip.Fixnum); ok {
			indices = append(indices, int(num))
		} else {
			slip.PanicType("subscript", a, "fixnum")
		}
	}
	al, ok := args[0].(slip.ArrayLike)
	if !ok || al.ElementType() != slip.BitSymbol {
		slip.PanicType("bit-array", args[0], "bit-array")
	}
	return al.Get(indices...)
}

// Place a value in the first position of a list or cons.
func (f *Bit) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	indices := make([]int, 0, len(args)-1)
	for _, a := range args[1:] {
		if num, ok := a.(slip.Fixnum); ok {
			indices = append(indices, int(num))
		} else {
			slip.PanicType("subscript", a, "fixnum")
		}
	}
	al, ok := args[0].(slip.ArrayLike)
	if !ok {
		slip.PanicType("array", args[0], "array")
	}
	al.Set(value, indices...)
}
