// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ChangeClass{Function: slip.Function{Name: "change-class", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.FunctionSymbol,
			Name: "change-class",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "An instance to change the class or flavor of.",
				},
				{
					Name: "new-class",
					Type: "class designator",
					Text: "A class or flavor designator for the new class or flavor of the instance.",
				},
				{Name: "&key"},
				{Name: "&allow-other-keys"},
			},
			Return: "instance",
			Text:   `__change-class__ changes the class or flavor of an instance.`,
		}, &slip.CLPkg)
}

// ChangeClass represents the change-class function.
type ChangeClass struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ChangeClass) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, -1)
	inst, ok := args[0].(slip.Instance)
	if !ok {
		slip.PanicType("instance", args[0], "instance")
	}
	_ = inst.Receive(s, ":change-class", args[1:], depth)

	return inst
}
