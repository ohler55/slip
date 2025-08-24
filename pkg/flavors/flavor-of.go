// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FlavorOf{Function: slip.Function{Name: "flavor-of", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "flavor-of",
			Args: []*slip.DocArg{
				{
					Name: "object",
					Type: "instance",
					Text: "The instance to get the flavor of.",
				},
			},
			Return: "flavor",
			Text:   `__flavor-of__ returns the _flavor_ of the _object_ or nil if the _object_ is not an _instance_.`,
			Examples: []string{
				"(flavor-of (make-instance 'strawberry)) => #<flavor strawberry>",
			},
		}, &Pkg)
}

// FlavorOf represents the makeInstance function.
type FlavorOf struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FlavorOf) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if inst, ok := args[0].(*Instance); ok {
		return inst.Type
	}
	return nil
}
