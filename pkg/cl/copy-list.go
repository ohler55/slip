// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CopyList{Function: slip.Function{Name: "copy-list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "copy-list",
			Args: []*slip.DocArg{
				{
					Name: "list",
					Type: "list",
					Text: "The list to copy.",
				},
			},
			Return: "list",
			Text: `__copy-list__ returns a copy of _list_ with all elements the
same as _list_. This is a shallow copy.`,
			Examples: []string{
				"(copy-list '(a b c)) => (a b c)",
				"(setq lst '(a b c))",
				"(eq lst (copy-list lst)) => nil",
				"(equal lst (copy-list lst)) => t",
			},
		}, &slip.CLPkg)
}

// CopyList represents the copy-list function.
type CopyList struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CopyList) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	var list slip.List
	switch ta := args[0].(type) {
	case nil:
		// leave as nil
	case slip.List:
		list = make(slip.List, len(ta))
		copy(list, ta)
	default:
		slip.PanicType("list", args[0], "list")
	}
	return list
}
