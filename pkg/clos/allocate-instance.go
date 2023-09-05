// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AllocateInstance{Function: slip.Function{Name: "allocate-instance", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "allocate-instance",
			Args: []*slip.DocArg{
				{
					Name: "flavor",
					Type: "symbol",
					Text: "The name of the flavor to make an instance of.",
				},
				{Name: slip.AmpRest},
				{
					Name: "options",
					Type: "object",
					Text: `The remaining arguments must be pairs of an init-option and value.
An init-value can be the variable name prefixed with a colon or a plist option. The _:init_
method is not called after the instance is created.`,
				},
			},
			Return: "instance",
			Text: `__allocate-instance__ makes an instance of _flavor_ with the provided
variable values and plist options.`,
			Examples: []string{
				"(allocate-instance 'strawberry :color 'red) => #<strawberry 123456>",
			},
		}, &Pkg)
}

// AllocateInstance represents the makeInstance function.
type AllocateInstance struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *AllocateInstance) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	c := classFromArg0(f, s, args, "allocate-instance")
	inst := c.MakeInstance()
	inst.Init(nil, args[1:], 0)

	return inst
}
