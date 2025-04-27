// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BitVectorP{Function: slip.Function{Name: "bit-vector-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bit-vector-p",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "boolean",
			Text:   `__bit-vector-p__ returns _true_ if _object_ is a _bit-vector_.`,
			Examples: []string{
				"(bit-vector-p #*1010) => t",
				"(bit-vector-p 5) => nil",
			},
		}, &slip.CLPkg)
}

// BitVectorP represents the bit-vector-p function.
type BitVectorP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BitVectorP) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	if _, ok := args[0].(*slip.BitVector); ok {
		return slip.True
	}
	return nil
}
