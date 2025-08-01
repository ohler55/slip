// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defSlotMissing() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := SlotMissing{Function: slip.Function{Name: "slot-missing", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "slot-missing",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "class",
					Text: "The class of the object.",
				},
				{
					Name: "object",
					Type: "instance",
					Text: "The instance that is missing the slot.",
				},
				{
					Name: "slot-name",
					Type: "symbol",
					Text: "The name of a slot.",
				},
				{
					Name: "operation",
					Type: "symbol",
					Text: "The operation attempted.",
				},
				{Name: "&optional"},
				{
					Name: "new-value",
					Type: "object",
					Text: "The new value if the operation is a __setf__.",
				},
			},
			Text: `__slot-missing__ raises an error.`,
		}, &Pkg)
}

// SlotMissing represents the slot-missing function.
type SlotMissing struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *SlotMissing) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 4, 5)
	// Ignore class as it is not used in forming the error message.
	slotName, ok := args[2].(slip.Symbol)
	if !ok {
		slip.PanicType("slot-name", args[2], "symbol")
	}
	if sym, ok2 := args[3].(slip.Symbol); ok2 {
		slotMissing(args[1], slotName, string(sym))
	}
	panic(slip.NewTypeError("operation", args[3], "symbol"))
}

func slotMissing(obj slip.Object, name slip.Symbol, op string) {
	if op == "setf" {
		slip.PanicCell(name,
			"When attempting to set the slot's value (%s), the slot %s is missing from the object %s.",
			op, name, obj)
	} else {
		slip.PanicCell(name,
			"When attempting to read the slot's value (%s), the slot %s is missing from the object %s.",
			op, name, obj)
	}
}
