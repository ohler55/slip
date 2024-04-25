// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AdjustableArrayP{Function: slip.Function{Name: "adjustable-array-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "adjustable-array-p",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to check.",
				},
			},
			Return: "boolean",
			Text:   `__adjustable-array-p__ returns true if _array_ is adjustable.`,
			Examples: []string{
				"(adjustable-array-p (make-array 3 :adjustable t)) => t",
			},
		}, &slip.CLPkg)
}

// AdjustableArrayP represents the adjustable-array-p function.
type AdjustableArrayP struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AdjustableArrayP) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.Array:
		if ta.Adjustable() {
			result = slip.True
		}
	case *slip.Vector:
		if ta.Adjustable() {
			result = slip.True
		}
	default:
		slip.PanicType("array", ta, "array")
	}
	return
}
