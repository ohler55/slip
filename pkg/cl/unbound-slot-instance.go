// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const instanceSymbol = slip.Symbol("instance")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UnboundSlotInstance{Function: slip.Function{Name: "unbound-slot-instance", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unbound-slot-instance",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "unbound-slot",
					Text: "The unbound-slot or subclass of unbound-slot to get the instance of.",
				},
			},
			Return: "object",
			Text: `__unbound-slot-instance__ returns the value of the _instance_ slot in the _condition_
which must be of type _unbound-slot_ or inherit from _unbound-slot_.`,
			Examples: []string{
				`(unbound-slot-instance (make-condition 'unbound-slot :instance 'test)) => test)`,
			},
		}, &slip.CLPkg)
}

// UnboundSlotInstance represents the unbound-slot-instance function.
type UnboundSlotInstance struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UnboundSlotInstance) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch cond := args[0].(type) {
	case slip.UnboundSlot:
		result = cond.Instance()
	case slip.Instance:
		var has bool
		if result, has = cond.SlotValue(instanceSymbol); !has {
			slip.PanicUnboundSlot(args[0], instanceSymbol, "")
		}
	default:
		slip.PanicUnboundSlot(args[0], instanceSymbol, "")
	}
	return
}
