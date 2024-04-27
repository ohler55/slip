// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Arrayp{Function: slip.Function{Name: "arrayp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "arrayp",
			Args: []*slip.DocArg{
				{
					Name: "array",
					Type: "array",
					Text: "The array to get the rank of.",
				},
			},
			Return: "boolean",
			Text:   `__arrayp__ returns the rank of _array_.`,
			Examples: []string{
				"(arrayp (make-array '(2 3)) => 2",
			},
		}, &slip.CLPkg)
}

// Arrayp represents the arrayp function.
type Arrayp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Arrayp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch args[0].(type) {
	case *slip.Array, *slip.Vector:
		result = slip.True
	}
	return
}
