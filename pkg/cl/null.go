// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Null{Function: slip.Function{Name: "null", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "null",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__null__ returns _t_ if _object_ is an empty list or _nil_ otherwise _nil_ is returned.`,
			Examples: []string{
				"(null 1.2) => nil",
				"(null '()) => t",
			},
		}, &slip.CLPkg)
}

// Null represents the null function.
type Null struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Null) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
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
