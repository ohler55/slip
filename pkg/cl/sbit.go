// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Sbit{Function: slip.Function{Name: "sbit", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "sbit",
			Args: []*slip.DocArg{
				{
					Name: "bit-array",
					Type: "simple-bit-array",
					Text: "The simple-bit-array to get a value from.",
				},
				{Name: "&rest"},
				{
					Name: "subscripts",
					Type: "fixnum",
					Text: "The indices into the array.",
				},
			},
			Return: "sbit",
			Text:   `__sbit__ returns the indexed bit element of _array_.`,
			Examples: []string{
				"(sbit (make-array '(2 3) :element-type 'sbit :initial-contents '((1 0 1) (0 1 0))) 0 2) => 1",
			},
		}, &slip.CLPkg)
}

// Sbit represents the sbit function.
type Sbit struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Sbit) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
	if !ok || al.ElementType() != slip.BitSymbol {
		slip.TypePanic(s, depth, "bit-array", args[0], "simple-bit-array")
	}
	if fpv, ok := al.(slip.FillPtrVector); ok && 0 <= fpv.FillPointer() {
		slip.TypePanic(s, depth, "bit-array", args[0], "simple-bit-array")
	}
	return al.Get(indices...)
}

// Place a value in the first position of a list or cons.
func (f *Sbit) Place(s *slip.Scope, args slip.List, value slip.Object) {
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
	if !ok || al.ElementType() != slip.BitSymbol {
		slip.TypePanic(s, 0, "bit-array", args[0], "simple-bit-array")
	}
	if fpv, ok := al.(slip.FillPtrVector); ok && 0 <= fpv.FillPointer() {
		slip.TypePanic(s, 0, "bit-array", args[0], "simple-bit-array")
	}
	al.Set(value, indices...)
}
