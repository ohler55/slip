// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := CopyTree{Function: slip.Function{Name: "copy-tree", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "copy-tree",
			Args: []*slip.DocArg{
				{
					Name: "tree",
					Type: "object",
					Text: "The object to copy.",
				},
			},
			Return: "object",
			Text: `__copy-tree__ returns the _tree_ if it is not a _cons_. If _tree_ is a
_cons_ then a __copy-tree__ is called on both the car and the cdr of the _cons_.`,
			Examples: []string{
				"(copy-tree '(a (b . 2) (c 3))) => (a (b . 2) (c 3))",
				"(setq tree '(a (b . 2) (c 3)))",
				"(eq (car tree) (car (copy-tree tree))) => t",
				"(equal tree (copy-tree tree)) => t",
			},
		}, &slip.CLPkg)
}

// CopyTree represents the copy-tree function.
type CopyTree struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *CopyTree) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)

	return copyTree(args[0])
}

func copyTree(v slip.Object) slip.Object {
	if list, ok := v.(slip.List); ok {
		dup := make(slip.List, len(list))
		for i, e := range list {
			dup[i] = copyTree(e)
		}
		return dup
	}
	return v
}
