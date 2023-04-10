// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Listp{Function: slip.Function{Name: "listp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "listp",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "object",
					Text: "The object to check.",
				},
			},
			Return: "nil",
			Text:   `__listp__ returns _true_ if _object_ is a list.`,
			Examples: []string{
				"(listp '(1 2)) => t",
				"(listp '()) => t",
				"(listp '(1 . 2)) => t",
				"(listp t) => nil",
			},
		}, &slip.CLPkg)
}

// Listp represents the listp function.
type Listp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Listp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch args[0].(type) {
	case nil, slip.List:
		return slip.True
	}
	return nil
}
