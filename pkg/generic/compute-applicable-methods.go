// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defComputeApplicableMethods() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ComputeApplicableMethods{Function: slip.Function{Name: "compute-applicable-methods", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "compute-applicable-methods",
			Args: []*slip.DocArg{
				{
					Name: "generic-function",
					Type: "generic-function",
					Text: `A generic-function or designator.`,
				},
				{
					Name: "function-arguments",
					Type: "list",
					Text: `A list of values to be used as specializers.`,
				},
			},
			Return: "list",
			Text: `__compute-applicable-methods__ returns a list of methods of the )generic-function_ that
match the _function-arguments_ as then specifiers.`,
		}, &Pkg)
}

// ComputeApplicableMethods represents the compute-applicable-methods function.
type ComputeApplicableMethods struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ComputeApplicableMethods) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(args[0], args, 2, 2)
	var aux *Aux
	a0 := args[0]
top:
	switch ta := a0.(type) {
	case slip.Symbol:
		a0 = slip.FindFunc(string(ta))
		goto top
	case *slip.FuncInfo:
		aux, _ = ta.Aux.(*Aux)
	}
	if aux == nil {
		slip.PanicType("generic-function", args[0], "symbol", "generic-function")
	}
	gfa, _ := args[1].(slip.List)
	if len(gfa) < aux.reqCnt {
		slip.PanicType("function-arguments", args[1], "list")
	}
	return aux.compMethList(gfa)
}
