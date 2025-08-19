// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defNoNextMethod() {
	fd := slip.FuncDoc{
		Name: "no-next-method",
		Kind: slip.GenericFunctionSymbol,
		Args: []*slip.DocArg{
			{
				Name: "generic-function",
				Type: "generic-function",
				Text: "A generic function on which no next method was found.",
			},
			{
				Name: "method",
				Type: "method",
				Text: "Method that had no next method.",
			},
			{Name: slip.AmpRest},
			{
				Name: "args",
				Type: "object",
				Text: "arguments to the function.",
			},
		},
		Return: "object",
		Text: `__no-next-method__ is a generic function that is called when _generic-function_
is invoked and no next method in an :around method is encountered.`,
	}
	aux := NewAux(&fd)
	md := slip.FuncDoc{
		Name:   fd.Name,
		Kind:   slip.MethodSymbol,
		Args:   fd.Args,
		Return: "object",
	}
	aux.methods["t"] = &slip.Method{
		Name:         fd.Name,
		Doc:          &md,
		Combinations: []*slip.Combination{{Primary: defaultNoNextMethCaller{}}},
	}
	Pkg.Define(
		func(args slip.List) slip.Object {
			f := NoNextMethod{
				Function: slip.Function{Name: "no-next-method", Args: args, SkipEval: []bool{true}},
				aux:      aux,
			}
			f.Self = &f
			return &f
		},
		&fd,
		aux,
	)
}

// NoNextMethod represents the class-name function.
type NoNextMethod struct {
	slip.Function
	aux *Aux
}

// Call the the function with the arguments provided.
func (f *NoNextMethod) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	var caller slip.Caller = defaultNoNextMethCaller{}
	var key []byte
	key = append(key, f.Hierarchy()[0]...)
	key = append(key, '|', 't')
	f.aux.moo.Lock()
	meth := f.aux.cache[string(key)]
	if meth == nil {
		if meth = f.aux.buildCacheMeth(args); meth != nil {
			f.aux.cache[string(key)] = meth
		}
	}
	f.aux.moo.Unlock()
	if meth != nil {
		caller = meth
	}
	return caller.Call(s, args, depth)
}

type defaultNoNextMethCaller struct{}

func (defaultNoNextMethCaller) Call(_ *slip.Scope, args slip.List, _ int) slip.Object {
	panic(slip.NewError("No next method for %s %s. %s", args[0], args[1], args[2:]))
}
