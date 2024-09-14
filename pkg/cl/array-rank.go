// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ArrayRank{Function: slip.Function{Name: "array-rank", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "array-rank",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the rank of.",
				},
			},
			Return: "fixnum",
			Text:   `__array-rank__ returns the rank of _array_.`,
			Examples: []string{
				"(array-rank (make-array '(2 3)) => 2",
			},
		}, &slip.CLPkg)
}

// ArrayRank represents the array-rank function.
type ArrayRank struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ArrayRank) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case *slip.Array:
		result = slip.Fixnum(ta.Rank())
	case *slip.Vector, slip.Octets:
		result = slip.Fixnum(1)
	default:
		slip.PanicType("array", ta, "array")
	}
	return
}
