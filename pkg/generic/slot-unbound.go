// Copyright (c) 2024, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defSlotUnbound() {
	fd := slip.FuncDoc{
		Name: "slot-unbound",
		Kind: slip.GenericFunctionSymbol,
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
	}
	aux := NewAux(&fd)
	md := slip.FuncDoc{
		Name:   fd.Name,
		Kind:   slip.MethodSymbol,
		Args:   fd.Args,
		Return: "object",
	}
	aux.AddMethod("t|t|t", &slip.Method{
		Name:         fd.Name,
		Doc:          &md,
		Combinations: []*slip.Combination{{Primary: defaultSlotUnboundCaller{}}},
	})
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := SlotUnbound{
				Function: slip.Function{Name: "slot-unbound", Args: args, SkipEval: []bool{true}},
				aux:      aux,
			}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}

// SlotUnbound represents the slot-unbound function.
type SlotUnbound struct {
	slip.Function
	aux *Aux
}

// Call the the function with the arguments provided.
func (f *SlotUnbound) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var caller slip.Caller = defaultSlotUnboundCaller{}
	key := buildSpecKey(args[:3])
	f.aux.moo.Lock()
	meth := f.aux.cache[key]
	if meth == nil {
		if meth = f.aux.buildCacheMeth(args); meth != nil {
			f.aux.cache[key] = meth
		}
	}
	f.aux.moo.Unlock()
	if meth != nil {
		caller = meth
	}
	return caller.Call(s, args, depth)
}

type defaultSlotUnboundCaller struct{}

func (defaultSlotUnboundCaller) Call(_ *slip.Scope, args slip.List, _ int) slip.Object {
	slip.ArgCountCheck(args[0], args, 3, 3)
	panic(slip.NewUnboundSlot(args[1], args[2], ""))
}
