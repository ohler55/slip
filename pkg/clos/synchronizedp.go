// Copyright (c) 2026, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defSynchronizedp() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Synchronizedp{Function: slip.Function{Name: "synchronizedp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "synchronizedp",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance to check whether it is currently mutex protected (synchronized).",
				},
			},
			Return: "boolean",
			Text:   `__synchronizedp__ return true if the _instance_ is synchronized.`,
			Examples: []string{
				"(synchronizedp (make-instance 'strawberry :size 'small)) => nil",
			},
		}, &Pkg)
}

// Synchronizedp represents the synchronizedp function.
type Synchronizedp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Synchronizedp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if inst, ok := args[0].(slip.Instance); ok {
		if inst.Synchronized() {
			result = slip.True
		}
	} else {
		slip.TypePanic(s, depth, "instance", args[0], "instance")
	}
	return
}
