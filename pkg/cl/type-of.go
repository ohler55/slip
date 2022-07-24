// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := TypeOf{Function: slip.Function{Name: "type-of", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "type-of",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__type-of__ returns the type of the object as a symbol.`,
			Examples: []string{
				"(type-of 1) => fixnum",
				"(type-of nil) => null",
			},
		}, &slip.CLPkg)
}

// TypeOf represents the type-of function.
type TypeOf struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *TypeOf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case nil:
		return slip.Symbol("null")
	case slip.List:
		if len(ta) == 0 {
			return slip.Symbol("null")
		}
	}
	return args[0].Hierarchy()[0]
}
