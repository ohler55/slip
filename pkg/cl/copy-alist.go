// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CopyAlist{Function: slip.Function{Name: "copy-alist", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "copy-alist",
			Args: []*slip.DocArg{
				{
					Name: "alist",
					Type: "association list",
					Text: "The association list to copy.",
				},
			},
			Return: "list",
			Text: `__copy-alist__ returns a copy of _alist_ with all _cons_ elements
copied and any non-_cons_ elements not reused.`,
			Examples: []string{
				"(copy-alist '((a . 1) (b . 2) (c . 3))) => ((a . 1) (b . 2) (c . 3))",
				"(setq alist '((a . 1) (b . 2) (c . 3)))",
				"(eq (car alist) (car (copy-alist alist))) => nil",
				"(equal alist (copy-alist alist)) => t",
			},
		}, &slip.CLPkg)
}

// CopyAlist represents the copy-alist function.
type CopyAlist struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CopyAlist) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	var alist slip.List
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.List:
		alist = make(slip.List, len(ta))
		for i, e := range ta {
			if list, ok := e.(slip.List); ok {
				dup := make(slip.List, len(list))
				copy(dup, list)
				alist[i] = dup
			} else {
				alist[i] = e
			}
		}
	default:
		slip.PanicType("list", args[0], "list")
	}
	return alist
}
