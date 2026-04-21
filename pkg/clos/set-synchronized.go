// Copyright (c) 2026, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defSetSynchronized() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SetSynchronized{Function: slip.Function{Name: "set-synchronized", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "set-synchronized",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance to set the synchronized state of.",
				},
				{
					Name: "active",
					Type: "boolean",
					Text: "The state to set the synchronized state to.",
				},
			},
			Return: "boolean",
			Text: `__set-synchronized__ sets the synchronized state of the instance. If _active_
is true then the instance is set to protect get and set slots with a mutex. If _active_ is false
then the instance is no longer mutex protected.`,
			Examples: []string{
				"(set-synchronized (make-instance 'strawberry :size 'small) t) => t",
			},
		}, &Pkg)
}

// SetSynchronized represents the set-synchronized function.
type SetSynchronized struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SetSynchronized) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	inst, ok := args[0].(slip.Instance)
	if !ok {
		slip.TypePanic(s, depth, "instance", args[0], "instance")
	}
	// To avoid changing a mutex if is is already set a check is made first.
	if args[1] != nil {
		if !inst.Synchronized() {
			inst.SetSynchronized(true)
		}
		result = slip.True
	} else {
		inst.SetSynchronized(false)
	}
	return
}
