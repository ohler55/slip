// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Nconc{Function: slip.Function{Name: "nconc", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "nconc",
			Args: []*slip.DocArg{
				{Name: "&rest"},
				{
					Name: "lists",
					Type: "list",
					Text: "The lists to concatenate.",
				},
			},
			Return: "list",
			Text: `__nconc__ concatenates the _lists_ modifying the left most list to include the rest.
Unlike common LISP the original lists are not always modified.`,
			Examples: []string{
				"(nconc '(a b) '(c d)) => (a b c d)",
				"(nconc) => nil",
			},
		}, &slip.CLPkg)
}

// Nconc represents the nconc function.
type Nconc struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Nconc) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	for i := len(args) - 1; 0 <= i; i-- {
		switch ta := args[i].(type) {
		case nil:
			// no change
		case slip.List:
			if len(ta) == 0 {
				// no change
				break
			}
			if result == nil {
				result = ta
				break
			}
			if _, ok := ta[len(ta)-1].(slip.Tail); ok {
				if list, ok := result.(slip.List); ok {
					ta = append(ta[:len(ta)-1], list...)
				} else {
					ta[len(ta)-1] = slip.Tail{Value: result}
				}
				result = ta
				break
			}
			if list, ok := result.(slip.List); ok {
				ta = append(ta, list...)
				result = ta
				break
			}
			ta = append(ta, slip.Tail{Value: result})
			result = ta
		default:
			if result == nil {
				result = ta
				break
			}
			slip.PanicType("list", ta, "list")
		}
	}
	return
}
