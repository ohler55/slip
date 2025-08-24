// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Emptyp{Function: slip.Function{Name: "emptyp", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "emptyp",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "String to check if empty.",
				},
			},
			Return: "boolean",
			Text:   `__emptyp__ returns true if _string_ is empty or nil.`,
			Examples: []string{
				`(emptyp "abc") => nil`,
				`(emptyp "") => t`,
				`(emptyp nil) => t`,
			},
		}, &Pkg)
}

// Emptyp represents the emptyp function.
type Emptyp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Emptyp) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	switch ta := args[0].(type) {
	case nil:
		result = slip.True
	case slip.String:
		if len(ta) == 0 {
			result = slip.True
		}
	}
	return
}
