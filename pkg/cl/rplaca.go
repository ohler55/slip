// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Rplaca{Function: slip.Function{Name: "rplaca", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "rplaca",
			Args: []*slip.DocArg{
				{
					Name: "cons",
					Type: "cons",
					Text: "The _list_ or _cons_ to replace the car of.",
				},
				{
					Name: "value",
					Type: "object",
					Text: "The value to replace the car of in the _cons_.",
				},
			},
			Return: "object",
			Text:   `__rplaca__ replaces the car of a _cons_ or _list_ with the provided _value_.`,
			Examples: []string{
				"(rplaca '(a b) 'x) => (x b)",
			},
		}, &slip.CLPkg)
}

// Rplaca represents the rplaca function.
type Rplaca struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Rplaca) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	list, ok := args[0].(slip.List)
	if !ok || len(list) == 0 {
		slip.PanicType("cons", args[0], "cons", "list")
	}
	list[0] = args[1]

	return list
}
