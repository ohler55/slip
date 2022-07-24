// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Numberp{Function: slip.Function{Name: "numberp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "numberp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__numberp__ returns _true_ if _object_ is a number.`,
			Examples: []string{
				"(numberp 4) => t",
				"(numberp 5.1) => nil",
			},
		}, &slip.CLPkg)
}

// Numberp represents the numberp function.
type Numberp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Numberp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.Number); ok {
		return slip.True
	}
	return nil
}
