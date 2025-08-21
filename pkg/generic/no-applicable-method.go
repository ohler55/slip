// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defNoApplicableMethod() {
	fd := slip.FuncDoc{
		Name: "no-applicable-method",
		Kind: slip.GenericFunctionSymbol,
		Args: []*slip.DocArg{
			{
				Name: "generic-function",
				Type: "generic-function",
				Text: "A generic function on which no applicable method was found.",
			},
			{Name: slip.AmpRest},
			{
				Name: "function-arguments",
				Type: "list",
				Text: "Arguments to the generic function.",
			},
		},
		Return: "object",
		Text: `__no-applicable-method__ is a generic function that is called when _generic-function_
is invoked and no method with the argument specializers is found.`,
	}
	aux := NewAux(&fd)
	md := slip.FuncDoc{
		Name:   fd.Name,
		Kind:   slip.MethodSymbol,
		Args:   fd.Args,
		Return: "object",
	}
	aux.AddMethod("t", &slip.Method{
		Name:         fd.Name,
		Doc:          &md,
		Combinations: []*slip.Combination{{Primary: defaultNoAppMethCaller{}}},
	})
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := NoApplicableMethod{
				Function: slip.Function{Name: "no-applicable-method", Args: args, SkipEval: []bool{true}},
				aux:      aux,
			}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}

// NoApplicableMethod represents the class-name function.
type NoApplicableMethod struct {
	slip.Function
	aux *Aux
}

// Call the the function with the arguments provided.
func (f *NoApplicableMethod) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	return f.aux.Call(f, s, args, depth)
}

type defaultNoAppMethCaller struct{}

func (defaultNoAppMethCaller) Call(_ *slip.Scope, args slip.List, _ int) slip.Object {
	panic(slip.NewNoApplicableMethodError(args[0], args[1:], ""))
}
