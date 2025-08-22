// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := VectorPushExtend{Function: slip.Function{Name: "vector-push-extend", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "vector-push-extend",
			Args: []*slip.DocArg{
				{
					Name: "new-element",
					Type: "object",
					Text: "The element to push-extend onto the vector.",
				},
				{
					Name: "vector",
					Type: "vector",
					Text: "The vector to push-extend a value to.",
				},
			},
			Return: "fixnum",
			Text: `__vector-push-extend__ returns index of the fill-pointer before the push.
If needed the vector is extended if the vector is adjustable.`,
			Examples: []string{
				"(vector-push-extend 'x (make-array '5 :fill-pointer 4)) => 4",
			},
		}, &slip.CLPkg)
}

// VectorPushExtend represents the vector-push-extend function.
type VectorPushExtend struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *VectorPushExtend) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	v, ok := args[1].(slip.FillPtrVector)
	if !ok {
		slip.TypePanic(s, depth, "vector", args[1], "vector with a fill-pointer.")
	}
	fp := v.FillPointer()
	if fp < 0 {
		slip.TypePanic(s, depth, "vector", v, "vector with a fill-pointer.")
	}
	result = slip.Fixnum(fp)
	v.Push(args[0])

	return
}
