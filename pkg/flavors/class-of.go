// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
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
					Text: "The instance to get the flavor of.",
				},
			},
			Return: "flavor",
			Text: `__class-of__ returns the _flavor_ of the _object_ or nil if the _object_ is not an _instance_.
This is an alias for __flavor-of__.`,
			Examples: []string{
				"(class-of (make-instance 'strawberry)) => #<flavor strawberry>",
			},
		}, &Pkg)
}

// ClassOf represents the makeInstance function.
type ClassOf struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassOf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	if inst, ok := args[0].(*Instance); ok {
		return inst.Flavor
	}
	return nil
}
