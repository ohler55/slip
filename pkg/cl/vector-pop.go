// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := VectorPop{Function: slip.Function{Name: "vector-pop", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "vector-pop",
			Args: []*slip.DocArg{
				{
					Name: "vector",
					Type: "vector",
					Text: "The vector to pop a value from.",
				},
			},
			Return: "object",
			Text: `__vector-pop__ returns the element at the current fill-pointer and decreases
the fill-pointer by one. If the fill-pointer is at zero an error is raised.`,
			Examples: []string{
				"(vector-pop (make-array '5 :fill-pointer 3 :initial-contents '(a b c d e))) => d",
			},
		}, &slip.CLPkg)
}

// VectorPop represents the vector-pop function.
type VectorPop struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *VectorPop) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	v, ok := args[0].(slip.FillPtrVector)
	if !ok {
		slip.PanicType("vector", args[0], "vector with a fill-pointer.")
	}
	fp := v.FillPointer()
	if fp < 0 {
		slip.PanicType("vector", v, "vector with a fill-pointer.")
	}
	if fp == 0 || v.Length() <= fp {
		slip.NewPanic("There is nothing left to pop.")
	}
	return v.Pop()
}
