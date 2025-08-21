// Copyright (c) 2025, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/generic"
)

func defInitializeInstance() {
	fd := slip.FuncDoc{
		Name: "initialize-instance",
		Kind: slip.GenericFunctionSymbol,
		Args: []*slip.DocArg{
			{
				Name: "instance",
				Type: "standard-object",
				Text: "A standard-object.",
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
		Text: `__initialize-instance__ is a generic function used to initialize _instance_. It is called
when an instance is created.`,
	}
	aux := generic.NewAux(&fd)
	md := slip.FuncDoc{
		Name:   fd.Name,
		Kind:   slip.MethodSymbol,
		Args:   fd.Args,
		Return: "instance",
	}
	aux.AddMethod("t", &slip.Method{
		Name:         fd.Name,
		Doc:          &md,
		Combinations: []*slip.Combination{{Primary: defaultInitializeInstanceCaller{}}},
	})
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := InitializeInstance{
				Function: slip.Function{Name: "initialize-instance", Args: args, SkipEval: []bool{true}},
				aux:      aux,
			}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}

// InitializeInstance represents the initialize-instance function.
type InitializeInstance struct {
	slip.Function
	aux *generic.Aux
}

// Call the the function with the arguments provided.
func (f *InitializeInstance) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return f.aux.Call(f, s, args, depth)
}

type defaultInitializeInstanceCaller struct{}

func (defaultInitializeInstanceCaller) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if fi := slip.FindFunc("shared-initialize"); fi != nil {
		args = append(slip.List{args[0], slip.True}, args[1:]...)
		_ = fi.Apply(s, args, depth+1)
	}
	return args[0]
}
