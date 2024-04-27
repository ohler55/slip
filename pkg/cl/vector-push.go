// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := VectorPush{Function: slip.Function{Name: "vector-push", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "vector-push",
			Args: []*slip.DocArg{
				{
					Name: "new-element",
					Type: "object",
					Text: "The element to push onto the vector.",
				},
				{
					Name: "vector",
					Type: "vector",
					Text: "The vector to push a value to.",
				},
			},
			Return: "fixnum",
			Text:   `__vector-push__ returns index of the fill-pointer before the push or _nil_ if the push failed.`,
			Examples: []string{
				"(vector-push 'x (make-array '5 :fill-pointer 2)) => 2",
			},
		}, &slip.CLPkg)
}

// VectorPush represents the vector-push function.
type VectorPush struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *VectorPush) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	v, ok := args[1].(*slip.Vector)
	if !ok {
		slip.PanicType("vector", args[1], "vector")
	}
	if v.FillPtr < 0 {
		slip.PanicType("vector", v, "vector with a fill-pointer.")
	}
	if v.Size() <= v.FillPtr {
		return nil
	}
	result = slip.Fixnum(v.FillPtr)
	v.Push(args[0])

	return
}
