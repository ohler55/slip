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
	specializers, _ := args[2].(slip.List)
	if len(specializers) != aux.reqCnt {
		slip.PanicType("specializers", args[2], "list")
	}
	var key []byte
	for i, sp := range specializers {
		if 0 < i {
			key = append(key, '|')
		}
		switch tsp := sp.(type) {
		case slip.Symbol:
			key = append(key, tsp...)
		case slip.Class:
			key = append(key, tsp.Name()...)
		default:
			slip.PanicType("specializer", tsp, "symbol", "class")
		}
	}
	m := aux.methods[string(key)]
	if m != nil {
		switch ta := args[1].(type) {
		case nil:
			if m.Combinations[0].Primary == nil {
				m = nil
			}
		case slip.List:
			switch len(ta) {
			case 0:
				if m.Combinations[0].Primary == nil {
					m = nil
				}
			case 1:
				switch ta[0] {
				case slip.Symbol(":before"):
					if m.Combinations[0].Before == nil {
						m = nil
					}
				case slip.Symbol(":after"):
					if m.Combinations[0].After == nil {
						m = nil
					}
				case slip.Symbol(":around"):
					if m.Combinations[0].Wrap == nil {
						m = nil
					}
				default:
					slip.PanicType("qualifiers", args[1], "()", "(:before)", "(:after)", "(:around)")
				}
			default:
				slip.PanicType("qualifiers", args[1], "()", "(:before)", "(:after)", "(:around)")
			}
		default:
			slip.PanicType("qualifiers", args[1], "()", "(:before)", "(:after)", "(:around)")
		}
	}
	if m == nil {
		if 3 < len(args) && args[3] != nil {
			slip.PanicError("Method for %s %s %s does not exist.", aux.docs.Name, args[1], specializers)
		}
		return nil
	}
	return m
}
