// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Acons{Function: slip.Function{Name: "acons", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "acons",
			Args: []*slip.DocArg{
				{
					Name: "key",
					Type: "object",
					Text: "The car to a new _cons_.",
				},
				{
					Name: "datum",
					Type: "object",
					Text: "The cdr to a new _cons_.",
				},
				{
					Name: "alist",
					Type: "list",
					Text: "The association list to prepend the new _cons_ to.",
				},
			},
			Return: "list",
			Text: `__cons__ prepends a new _cons_ with a car of _key_ and a cdr of _datum_
to _alist_. The new list is returned.`,
			Examples: []string{
				"(acons 'a 2 '((b . 2))) => ((a . 1) (b . 2))",
			},
		}, &slip.CLPkg)
}

// Acons represents the acons function.
type Acons struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Acons) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 3, 3)
	cons := slip.List{args[0], slip.Tail{Value: args[1]}}
	switch alist := args[2].(type) {
	case nil:
		return slip.List{cons}
	case slip.List:
		return append(slip.List{cons}, alist...)
	default:
		return slip.List{cons, slip.Tail{Value: alist}}
	}
}
