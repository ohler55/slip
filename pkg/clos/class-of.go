// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defClassOf() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClassOf{Function: slip.Function{Name: "class-of", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "class-of",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "instance",
					Text: "The instance to get the class or flavor of.",
				},
			},
			Return: "flavor",
			Text: `__class-of__ returns the _class_ or _flavor_ of the _object_ or nil if
the _object_ is not an _instance_.`,
			Examples: []string{
				"(class-of (make-instance 'strawberry)) => #<flavor strawberry>",
				"(class-of 7) => #<class fixnum>",
			},
		}, &Pkg)
}

// ClassOf represents the class-of function.
type ClassOf struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassOf) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if inst, ok := args[0].(slip.Instance); ok {
		return inst.Class()
	}
	return nil
}
