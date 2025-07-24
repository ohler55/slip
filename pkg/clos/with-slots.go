// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defWithSlots() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WithSlots{Function: slip.Function{Name: "with-slots", Args: args, SkipEval: []bool{true, false, true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.FunctionSymbol,
			Name: "with-slots",
			Args: []*slip.DocArg{
				{
					Name: "slot-entries",
					Type: "list",
					Text: "A list of slot-entry which can be either a single symbol or a pair of symbols.",
				},
				{
					Name: "instance",
					Type: "instance",
					Text: "An instance for binding variable to slots.",
				},
				{Name: "&rest"},
				{
					Name: "forms",
					Type: "form",
					Text: "Forms to evaluate with the scope of the bound variables.",
				},
			},
			Return: "object",
			Text: `__with-slots__ evaluates _forms_ in then scope formed by binding the _slot-entrys_
variables with a slot on the instance. Changes in the bound variables are reflected in the slot values
of _instance_.`,
		}, &Pkg)
}

// WithSlots represents the with-slots function.
type WithSlots struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *WithSlots) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, -1)
	inst, ok := args[1].(slip.Instance)
	if !ok {
		slip.PanicType("instance", args[1], "instance")
	}
	ns := s.NewScope()
	d2 := depth + 1
	var entries slip.List
	if entries, ok = args[0].(slip.List); !ok {
		slip.PanicType("slot-entries", args[0], "list")
	}
	for _, entry := range entries {
		switch te := entry.(type) {
		case slip.Symbol:
			if _, has := inst.SlotValue(te); has {
				ns.UnsafeLet(te, &slip.Ref{Instance: inst, Key: te})
			} else {
				slotMissing(inst, te, "with-slots")
			}
		case slip.List:
			if len(te) != 2 {
				slip.PanicType("slot-entry", entry, "symbol", "list of two symbols")
			}
			var (
				vname slip.Symbol
				sname slip.Symbol
			)
			if vname, ok = te[0].(slip.Symbol); !ok {
				slip.PanicType("slot-entry", entry, "symbol", "list of two symbols")
			}
			if sname, ok = te[1].(slip.Symbol); !ok {
				slip.PanicType("slot-entry", entry, "symbol", "list of two symbols")
			}
			if _, has := inst.SlotValue(sname); has {
				ns.UnsafeLet(vname, &slip.Ref{Instance: inst, Key: sname})
			} else {
				slotMissing(inst, sname, "with-slots")
			}
		default:
			slip.PanicType("slot-entry", entry, "symbol", "list")
		}
	}
	for i := 2; i < len(args); i++ {
		result = slip.EvalArg(ns, args, i, d2)
	}
	return
}
