// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Svref{Function: slip.Function{Name: "svref", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "svref",
			Args: []*slip.DocArg{
				{
					Name: "simple-vector",
					Type: "simple-vector",
					Text: "The vector to get a value from.",
				},
				{
					Name: "index",
					Type: "fixnum",
					Text: "The index into the vector.",
				},
			},
			Return: "object",
			Text:   `__svref__ returns the indexed element of _vector_.`,
			Examples: []string{
				"(svref (make-array 4 :initial-contents '(a b c d)) 2) => c",
			},
		}, &slip.CLPkg)
}

// Svref represents the svref function.
type Svref struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Svref) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("index", args[1], "fixnum")
	}
	var vl slip.VectorLike
	if vl, ok = args[0].(slip.VectorLike); ok {
		et := vl.ElementType()
		if et == slip.TrueSymbol || et == slip.BitSymbol || et == slip.OctetSymbol {
			if fpv, ok2 := vl.(slip.FillPtrVector); !ok2 || fpv.FillPointer() < 0 {
				return vl.Get(int(index))
			}
		}
	}
	panic(slip.NewTypeError(s, depth, "simple-vector", args[0], "simple-vector"))
}

// Place a value in the first position of a list or cons.
func (f *Svref) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	index, ok := args[1].(slip.Fixnum)
	if !ok {
		slip.PanicType("index", args[1], "fixnum")
	}
	var vl slip.VectorLike
	if vl, ok = args[0].(slip.VectorLike); ok {
		et := vl.ElementType()
		if et == slip.TrueSymbol || et == slip.BitSymbol || et == slip.OctetSymbol {
			if fpv, ok2 := vl.(slip.FillPtrVector); !ok2 || fpv.FillPointer() < 0 {
				vl.Set(value, int(index))
				return
			}
		}
	}
	panic(slip.NewTypeError(s, 0, "simple-vector", args[0], "simple-vector"))
}
