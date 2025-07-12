// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defSlotUnbound() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlotUnbound{Function: slip.Function{Name: "slot-unbound", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slot-unbound",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "class",
					Text: "The class of the object.",
				},
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance for the unbound slot.",
				},
				{
					Name: "slot-name",
					Type: "symbol",
					Text: "The name of a slot.",
				},
			},
			Text: `__slot-unbound__ raises an unbound-slot error.`,
		}, &Pkg)
}

// SlotUnbound represents the slot-unbound function.
type SlotUnbound struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SlotUnbound) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 3)
	// Ignore class as it is not used in forming the error message.
	if slotName, ok := args[2].(slip.Symbol); ok {
		slip.PanicUnboundSlot(args[1], slotName, "")
	}
	panic(slip.NewTypeErrorObject("slot-name", args[2], "symbol"))
}
