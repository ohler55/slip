// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rplacd{Function: slip.Function{Name: "rplacd", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "rplacd",
			Args: []*slip.DocArg{
				{
					Name: "cons",
					Type: "cons",
					Text: "The _list_ or _cons_ to replace the cdr of.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to replace the cdr of in the _cons_.",
				},
			},
			Return: "object",
			Text:   `__rplacd__ replaces the cdr of a _cons_ or _list_ with the provided _value_.`,
			Examples: []string{
				"(rplacd '(a b) 'x) => (a . x)",
				"(rplacd '(a . b) 'x) => (a . x)",
				"(rplacd '(a) 'x) => (a . x)",
			},
		}, &slip.CLPkg)
}

// Rplacd represents the rplacd function.
type Rplacd struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rplacd) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	list, ok := args[0].(slip.List)
	if !ok || len(list) == 0 {
		slip.PanicType("cons", args[0], "cons", "list")
	}
	if 1 < len(list) {
		list = list[:2]
		if a2, ok2 := args[1].(slip.List); ok2 {
			list = append(list[:1], a2...)
		} else {
			list[1] = slip.Tail{Value: args[1]}
		}
	} else if a2, ok2 := args[1].(slip.List); ok2 {
		list = append(list, a2...)
	} else {
		list = append(list, slip.Tail{Value: args[1]})
	}
	return list
}
