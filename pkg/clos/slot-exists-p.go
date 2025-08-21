// Copyright (c) 2024, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defSlotExistsp() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlotExistsp{Function: slip.Function{Name: "slot-exists-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slot-exists-p",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "The instance to check for a slot existence.",
				},
				{
					Name: "slot-name",
					Type: "symbol",
					Text: "The name of a slot.",
				},
			},
			Return: "boolean",
			Text:   `__slot-exists-p__ returns true if the _instance_ has the named variable.`,
			Examples: []string{
				"(slot-exists-p (make-instance 'strawberry) 'size) => t",
			},
		}, &Pkg)
}

// SlotExistsp represents the slot-exists-p function.
type SlotExistsp struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SlotExistsp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	sym, ok := args[1].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "slot-name", args[1], "symbol")
	}
	if inst, ok := args[0].(slip.Instance); ok {
		if _, has := inst.SlotValue(sym); has {
			result = slip.True
		}
	}
	return
}
