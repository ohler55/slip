// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FillPointer{Function: slip.Function{Name: "fill-pointer", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "fill-pointer",
			Args: []*slip.DocArg{
				{
					Name: "vector",
					Type: "vector",
					Text: "The vector to get the fill-pointer from.",
				},
			},
			Return: "fixnum",
			Text:   `__fill-pointer__ returns the fill-pointer of _vector_.`,
			Examples: []string{
				"(fill-pointer (make-array 5) :fill-pointer 3) => 3",
			},
		}, &slip.CLPkg)
}

// FillPointer represents the fill-pointer function.
type FillPointer struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FillPointer) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if v, ok := args[0].(slip.VectorLike); ok {
		fp := v.FillPointer()
		if 0 <= fp {
			result = slip.Fixnum(fp)
		} else {
			slip.PanicType("vector", args[0], "vector with a fill-pointer")
		}
	} else {
		slip.PanicType("vector", args[0], "vector with a fill-pointer")
	}
	return
}

// Place a value in the first position of a list or cons.
func (f *FillPointer) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	if v, ok := args[0].(slip.VectorLike); ok {
		fp := v.FillPointer()
		if 0 <= fp {
			if num, ok2 := value.(slip.Fixnum); ok2 && 0 <= num && int(num) < v.Length() {
				v.SetFillPointer(int(num))
			} else {
				slip.PanicType("fill-pointer", value, "fixnum")
			}
		} else {
			slip.PanicType("vector", args[0], "vector with a fill-pointer")
		}
	} else {
		slip.PanicType("vector", args[0], "vector with a fill-pointer")
	}
}
