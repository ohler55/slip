// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SimpleVectorP{Function: slip.Function{Name: "simple-vector-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "simple-vector-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__simple-vector-p__ returns true if _object_ is a _vector_ with no fill-pointer.`,
			Examples: []string{
				"(simple-vector-p (make-array 3 :fill-pointer nil)) => t",
			},
		}, &slip.CLPkg)
}

// SimpleVectorP represents the simple-vector-p function.
type SimpleVectorP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SimpleVectorP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if vl, ok := args[0].(slip.VectorLike); ok {
		et := vl.ElementType()
		if et == slip.TrueSymbol || et == slip.BitSymbol || et == slip.OctetSymbol {
			if fpv, ok2 := vl.(slip.FillPtrVector); !ok2 || fpv.FillPointer() < 0 {
				return slip.True
			}
		}
	}
	return nil
}
