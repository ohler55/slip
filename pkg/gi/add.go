// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Add{Function: slip.Function{Name: "add", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "add",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The list to be appended to.",
				},
				{Name: "&rest"},
				{
					Name: "objects",
					Type: "object",
					Text: "The objects to be appended to the _list_.",
				},
			},
			Return: "list",
			Text: `__add__ appends to the _list_ potentiallt modifying the _list_.
__add__ is an addition to common LISP.`,
			Examples: []string{
				"(add '(a b) 'c 'd) => (a b c d)",
				"(add nil) => nil",
			},
		}, &slip.CLPkg)
}

// Add represents the add function.
type Add struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Add) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, -1)
	var list slip.List
	switch ta := args[0].(type) {
	case nil:
		// leave as is
	case slip.List:
		list = ta
	default:
		slip.PanicType("list", ta, "list")
	}
	return append(list, args[1:]...)
}
