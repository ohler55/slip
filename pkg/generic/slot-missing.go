// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defSlotMissing() {
	fd := slip.FuncDoc{
		Name: "slot-missing",
		Kind: slip.GenericFunctionSymbol,
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
		Text: `__slot-missing__ default method raises a cell-error.`,
	}
	aux := NewAux(&fd)
	md := slip.FuncDoc{
		Name:   fd.Name,
		Kind:   slip.MethodSymbol,
		Args:   fd.Args,
		Return: "object",
	}
	aux.AddMethod("t|t|t|t", &slip.Method{
		Name:         fd.Name,
		Doc:          &md,
		Combinations: []*slip.Combination{{Primary: defaultSlotMissingCaller{}}},
	})
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := SlotMissing{
				Function: slip.Function{Name: "slot-missing", Args: args, SkipEval: []bool{true}},
				aux:      aux,
			}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}

// SlotMissing represents the slot-missing function.
type SlotMissing struct {
	slip.Function
	aux *Aux
}

// Call the the function with the arguments provided.
func (f *SlotMissing) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return f.aux.Call(f, s, args, depth)
}

type defaultSlotMissingCaller struct{}

func (defaultSlotMissingCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(args[3], args, 4, 5)
	var cond slip.Object
	// Ignore class as it is not used in forming the error message.
	slotName, ok := args[2].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "slot-name", args[2], "symbol")
	}
	if sym, ok2 := args[3].(slip.Symbol); ok2 && string(sym) == "setf" {
		cond = slip.NewCellError(
			slotName,
			"When attempting to set the slot's value (%s), the slot %s is missing from the object %s.",
			sym,
			slotName,
			args[1])
	} else {
		cond = slip.NewCellError(
			slotName,
			"When attempting to read the slot's value (%s), the slot %s is missing from the object %s.",
			args[3],
			slotName,
			args[1])
	}
	panic(cond)
}
