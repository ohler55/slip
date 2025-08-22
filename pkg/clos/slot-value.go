// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defSlotValue() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlotValue{Function: slip.Function{Name: "slot-value", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slot-value",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance to get a slot value of.",
				},
				{
					Name: "slot-name",
					Type: "symbol",
					Text: "The name of a slot.",
				},
			},
			Text: `__slot-value__ returns the value of the _slot-name_ of the _instance_.`,
			Examples: []string{
				"(slot-value (make-instance 'strawberry) 'size) => medium",
			},
		}, &Pkg)
}

// SlotValue represents the slot-value function.
type SlotValue struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SlotValue) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "slot-name", args[1], "symbol")
	}
	var has bool
	if inst, ok := args[0].(slip.Instance); ok {
		if result, has = inst.SlotValue(sym); !has {
			return slotMissing(s, inst, sym, "slot-value")
		}
		if result == slip.Unbound {
			if fi := slip.FindFunc("slot-unbound"); fi != nil {
				args := slip.List{
					slip.FindClass(string(inst.Hierarchy()[0])),
					inst,
					sym,
				}
				f, _ := fi.Create(args).(slip.Funky)

				result = f.Caller().Call(s, args, depth)
			}
		}
	} else {
		return slotMissing(s, args[0], sym, "slot-value")
	}
	return
}

// Place a value in the slot of an instance.
func (f *SlotValue) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.CheckArgCount(s, 0, f, args, 2, 2)
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, 0, "slot-name", args[1], "symbol")
	}
	if inst, ok := args[0].(slip.Instance); ok {
		if !inst.SetSlotValue(sym, value) {
			_ = slotMissing(s, inst, sym, "setf")
		}
	} else {
		_ = slotMissing(s, args[0], sym, "slot-value")
	}
}
