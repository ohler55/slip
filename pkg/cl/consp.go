// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Consp{Function: slip.Function{Name: "consp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "consp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__consp__ returns _true_ if _object_ is a cons otherwise nil is returned.`,
			Examples: []string{
				"(consp 1.2) => nil",
				"(consp '(1 2)) => t",
			},
		}, &slip.CLPkg)
}

// Consp represents the consp function.
type Consp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Consp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if list, ok := args[0].(slip.List); ok && 0 < len(list) {
		return slip.True
	}
	return nil
}
