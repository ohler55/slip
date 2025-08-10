// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

func defFindMethod() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindMethod{Function: slip.Function{Name: "find-method", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-method",
			Args: []*slip.DocArg{
				{
					Name: "generic-function",
					Type: "generic-function",
					Text: `A generic-function or designator.`,
				},
				{
					Name: "method-qualifiers",
					Type: "list",
					Text: `A list of method qualifiers.`,
				},
				{
					Name: "specializers",
					Type: "list",
					Text: `A list of classes or type to be used as specializers.
Deviating from common lisp, type names are also allowed`,
				},
				{Name: "&optional"},
				{
					Name: "errorp",
					Type: "boolean",
					Text: `If true and error is raised if no method is found otherwise _nil_ is returned.`,
				},
			},
			Return: "method",
			Text: `__find-method__ returns a method of the generic-function_ that
matchs the _specializers_ as well as _qualifiers_.`,
		}, &Pkg)
}

// FindMethod represents the find-method function.
type FindMethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FindMethod) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(args[0], args, 3, 4)
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

	// TBD qualifier either empty list or list of 1 that is (:before), (:after), or (:around)

	gfa, _ := args[2].(slip.List)
	if len(gfa) != aux.reqCnt {
		slip.PanicType("specializers", args[2], "list")
	}

	// TBD

	// TBD form key and lookup, then find qualifier match

	return nil
}
