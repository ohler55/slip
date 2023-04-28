// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Not{Function: slip.Function{Name: "not", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "not",
			Args: []*slip.DocArg{
				{
					Name: "value",
					Type: "object",
					Text: "The value to check for _nil_ or not.",
				},
			},
			Return: "nil",
			Text:   `__not__ returns _t_ if _object_ is an empty list or _nil_ otherwise _nil_ is returned.`,
			Examples: []string{
				"(not 1.2) => nil",
				"(not '()) => t",
			},
		}, &slip.CLPkg)
}

// Not represents the not function.
type Not struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Not) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.List:
		if len(ta) == 0 {
			return slip.True
		}
	case nil:
		return slip.True
	}
	return nil
}
