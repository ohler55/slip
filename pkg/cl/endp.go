// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Endp{Function: slip.Function{Name: "endp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "endp",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: `The list to check the end of.`,
				},
			},
			Return: "boolean",
			Text:   `__endp__ returns __t__ if _list_ is empty or __nil__.`,
			Examples: []string{
				`(endp nil) => t`,
				`(endp '()) => t`,
				"(endp '(a b)) => nil",
			},
		}, &slip.CLPkg)
}

// Endp represents the endp function.
type Endp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Endp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	switch ta := args[0].(type) {
	case nil:
		result = slip.True
	case slip.List:
		if len(ta) == 0 {
			result = slip.True
		}
	default:
		slip.PanicType("list", ta, "list")
	}
	return
}
