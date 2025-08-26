// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defChangeClass() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ChangeClass{Function: slip.Function{Name: "change-class", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.FunctionSymbol,
			Name: "change-class",
			Args: []*slip.DocArg{
				{
					Name: "instance",
					Type: "instance",
					Text: "An instance to change the class or flavor of.",
				},
				{
					Name: "new-class",
					Type: "class designator",
					Text: "A class or flavor designator for the new class or flavor of the instance.",
				},
				{Name: "&key"},
				{Name: "&allow-other-keys"},
			},
			Return: "instance",
			Text:   `__change-class__ changes the class or flavor of an instance.`,
		}, &Pkg)
}

// ChangeClass represents the change-class function.
type ChangeClass struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ChangeClass) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, -1)
	inst, ok := args[0].(slip.Instance)
	if !ok {
		slip.TypePanic(s, depth, "instance", args[0], "instance")
	}
	var class slip.Class
	switch ta := args[1].(type) {
	case slip.Symbol:
		if class = slip.FindClass(string(ta)); class == nil {
			slip.ClassNotFoundPanic(s, depth, ta, "Class %s not found.", ta)
		}
	case slip.Class:
		class = ta
	default:
		slip.TypePanic(s, depth, "new-class", ta, "symbol", "flavor", "class")
	}
	if class.Metaclass() != inst.Class().Metaclass() {
		slip.ErrorPanic(s, depth, "Can not change the class of an instance of a %s class to a %s class.",
			inst.Class().Metaclass(), class.Metaclass())
	}
	args = args[2:]
	dup := inst.Dup()
	switch ti := inst.(type) {
	case *StandardObject:
		ti.Type = class.(isStandardClass)
		ti.vars = map[string]slip.Object{}
		sdm := ti.Type.slotDefMap()
		for name, sd := range sdm {
			sym := slip.Symbol(name)
			if v, has := dup.SlotValue(sym); has {
				ti.vars[name] = v
			} else {
				var inited bool
				for _, key := range sd.initargs {
					if v, has = slip.GetArgsKeyValue(args, key); has {
						ti.vars[name] = v
						inited = true
						break
					}
				}
				if !inited {
					ti.vars[name] = sd.initform
				}
			}
		}
		// TBD call generic update-instance-for-different-class
	case *flavors.Instance:
		ti.ChangeFlavor(class.(*flavors.Flavor))
		_ = ti.Receive(s, ":update-instance-for-different-class", append(slip.List{dup}, args...), depth)
	}
	return inst
}
