// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SimpleBitVectorP{Function: slip.Function{Name: "simple-bit-vector-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "simple-bit-vector-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__simple-bit-vector-p__ returns _true_ if _object_ is a _simple-bit-vector_.`,
			Examples: []string{
				"(simple-bit-vector-p #*1010) => t ;; the default bit-vector is a simple-bit-vector",
				`(simple-bit-vector-p (make-array 4 :element-type 'bit :fill-pointer 3)) => nil`,
			},
		}, &slip.CLPkg)
}

// SimpleBitVectorP represents the simple-bit-vector-p function.
type SimpleBitVectorP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *SimpleBitVectorP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if bv, ok := args[0].(*slip.BitVector); ok && bv.FillPtr < 0 {
		return slip.True
	}
	return nil
}
