// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/generic"
)

func defSharedInitialize() {
	fd := slip.FuncDoc{
		Name: "shared-initialize",
		Kind: slip.GenericFunctionSymbol,
		Args: []*slip.DocArg{
			{
				Name: "instance",
				Type: "standard-object",
				Text: "A standard-object.",
			},
			{
				Name: "slot-names",
				Type: "list|t",
				Text: "Slot names to initialize or __t__ to initialze all.",
			},
			{Name: slip.AmpRest},
			{
				Name: "initargs",
				Type: "list",
				Text: "A list of keywords and value pairs.",
			},
			{Name: slip.AmpKey},
			{Name: slip.AmpAllowOtherKeys},
		},
		Return: "instance",
		Text: `__shared-initialize__ is a generic function used to fill slots of _instance_. It is called
when an instance is created by the __initialize-instance__ function.`,
	}
	aux := generic.NewAux(&fd)
	md := slip.FuncDoc{
		Name:   fd.Name,
		Kind:   slip.MethodSymbol,
		Args:   fd.Args,
		Return: "object",
	}
	aux.AddMethod("t|t", &slip.Method{
		Name:         fd.Name,
		Doc:          &md,
		Combinations: []*slip.Combination{{Primary: defaultSharedInitializeCaller{}}},
	})
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := SharedInitialize{
				Function: slip.Function{Name: "shared-initialize", Args: args, SkipEval: []bool{true}},
				aux:      aux,
			}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}

// SharedInitialize represents the shared-initialize function.
type SharedInitialize struct {
	slip.Function
	aux *generic.Aux
}

// Call the the function with the arguments provided.
func (f *SharedInitialize) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return f.aux.Call(f, s, args, depth)
}

type defaultSharedInitializeCaller struct{}

func (defaultSharedInitializeCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	obj, ok := args[0].(*StandardObject)
	if !ok {
		return nil
	}
	// args[1], slot-names is ignored
	if args, ok = args[2].(slip.List); !ok {
		return nil
	}
	nameMap := map[string]string{}
	argMap := map[string]slip.Object{}
	fillMapFromKeyArgs(args, argMap)
	for k, v := range argMap {
		sd := obj.Type.initArgDef(k)
		if sd == nil {
			slip.NewPanic("%s is not a valid initarg for %s.", k, obj.Type.Name())
		}
		if n, has := nameMap[sd.name]; has {
			slip.NewPanic("Duplicate initarg (%s) for slot %s. %s already specified.", sd.name, k, n)
		}
		obj.setSlot(sd, v)
		nameMap[sd.name] = k
	}
	for k, v := range obj.Type.defaultsMap() {
		sd := obj.Type.initArgDef(k)
		if _, has := nameMap[sd.name]; !has {
			if v == nil {
				obj.setSlot(sd, nil)
			} else {
				obj.setSlot(sd, v.Eval(s, depth+1))
			}
			nameMap[sd.name] = k
		}
	}
	for k, sd := range obj.Type.initFormMap() {
		if _, has := nameMap[k]; !has {
			// If in the initForms then initform will not be nil.
			obj.setSlot(sd, sd.initform.Eval(s, depth+1))
		}
	}
	return obj
}
