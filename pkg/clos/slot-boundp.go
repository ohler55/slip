// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defSlotBoundp() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlotBoundp{Function: slip.Function{Name: "slot-boundp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slot-boundp",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance to check whether a slot bound.",
				},
				{
					Name: "slot-name",
					Type: "symbol",
					Text: "The name of a slot.",
				},
			},
			Return: "boolean",
			Text:   `__slot-boundp__ return true if the variable _slot-name_ is bound in the _instance_.`,
			Examples: []string{
				"(slot-boundp (make-instance 'strawberry :size 'small) 'size) => t",
			},
		}, &Pkg)
}

// SlotBoundp represents the slot-boundp function.
type SlotBoundp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SlotBoundp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.PanicType("slot-name", args[1], "symbol")
	}
	if inst, ok := args[0].(*flavors.Instance); ok {
		if v, has := inst.LocalGet(sym); has {
			if v != slip.Unbound {
				result = slip.True
			}
		} else {
			slotMissing(inst, sym, "slot-boundp")
		}
	} else {
		slotMissing(args[0], sym, "slot-boundp")
	}
	return
}
