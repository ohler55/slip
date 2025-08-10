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
			Examples: []string{
				`(defgeneric quux (x y) (:method :before ((x real) (y real)) (print "before"))`,
				`(find-method 'quux '(:before) '(real real)) => #<method quux :before ((x real) (y real)) {1234}>`,
			},
		}, &Pkg)
}

// FindMethod represents the find-method function.
type FindMethod struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FindMethod) Call(s *slip.Scope, args slip.List, depth int) (meth slip.Object) {
	slip.ArgCountCheck(f, args, 3, 4)
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
			if slip.True == tsp {
				key = append(key, 't')
			} else {
				slip.PanicType("specializer", tsp, "symbol", "class", "t")
			}
		}
	}
	var qual string
	switch ta := args[1].(type) {
	case nil:
		qual = ":primary"
	case slip.List:
		switch len(ta) {
		case 0:
			qual = ":primary"
		case 1:
			sym, _ := ta[0].(slip.Symbol)
			qual = string(sym)
		}
	}
	if m := aux.methods[string(key)]; m != nil {
		switch qual {
		case ":primary":
			if m.Combinations[0].Primary != nil {
				meth = &slip.Method{
					Name:         m.Name,
					Doc:          m.Doc,
					Combinations: []*slip.Combination{{Primary: m.Combinations[0].Primary}},
				}
			}
		case ":before":
			if m.Combinations[0].Before != nil {
				meth = &slip.Method{
					Name:         m.Name,
					Doc:          m.Doc,
					Combinations: []*slip.Combination{{Before: m.Combinations[0].Before}},
				}
			}
		case ":after":
			if m.Combinations[0].After != nil {
				meth = &slip.Method{
					Name:         m.Name,
					Doc:          m.Doc,
					Combinations: []*slip.Combination{{After: m.Combinations[0].After}},
				}
			}
		case ":around":
			if m.Combinations[0].Wrap != nil {
				meth = &slip.Method{
					Name:         m.Name,
					Doc:          m.Doc,
					Combinations: []*slip.Combination{{Wrap: m.Combinations[0].Wrap}},
				}
			}
		default:
			slip.PanicType("qualifiers", args[1], "()", "(:before)", "(:after)", "(:around)")
		}
	}
	if meth == nil {
		if 3 < len(args) && args[3] != nil {
			slip.PanicError("Method for %s %s %s does not exist.", aux.docs.Name, args[1], specializers)
		}
	}
	return
}
