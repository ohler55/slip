// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defSlotMakunbound() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlotMakunbound{Function: slip.Function{Name: "slot-makunbound", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slot-makunbound",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance to make a slot unbound for.",
				},
				{
					Name: "slot-name",
					Type: "symbol",
					Text: "The name of a slot.",
				},
			},
			Text: `__slot-makunbound__ makes the variable _slot-name_ unbound in the _instance_.`,
			Examples: []string{
				"(slot-makunbound (make-instance 'strawberry) 'size)",
			},
		}, &Pkg)
}

// SlotMakunbound represents the slot-makunbound function.
type SlotMakunbound struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SlotMakunbound) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "slot-name", args[1], "symbol")
	}
	if inst, ok := args[0].(slip.Instance); ok {
		if !inst.SetSlotValue(sym, slip.Unbound) {
			return slotMissing(s, inst, sym, "slot-makunbound", depth)
		}
	} else {
		return slotMissing(s, args[0], sym, "slot-makunbound", depth)
	}
	return nil
}
