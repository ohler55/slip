// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Floatp{Function: slip.Function{Name: "floatp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "floatp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__floatp__ returns _true_ if _object_ is a float.`,
			Examples: []string{
				"(floatp 4.2) => t",
				"(floatp 5) => nil",
			},
		}, &slip.CLPkg)
}

// Floatp represents the floatp function.
type Floatp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Floatp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if _, ok := args[0].(slip.Float); ok {
		return slip.True
	}
	return nil
}
