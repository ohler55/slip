// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import "github.com/ohler55/slip"

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Cons{Function: slip.Function{Name: "cons", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "cons",
			Args: []*slip.DocArg{
				{Name: "object-1", Type: "object"},
				{Name: "object-2", Type: "object"},
			},
			Return: "list",
			Text: `__cons__ prepends _object-1_ to _object-2_ if _object-2_ is a list or if _object-2_
is not a list then a _cons_ is formed and returned.`,
			Examples: []string{
				"(cons 1 2) => (1 . 2)",
				"(cons 1 '(2)) => (1 2)",
			},
		}, &slip.CLPkg)
}

// Cons represents the cons function.
type Cons struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Cons) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 2 {
		slip.PanicArgCount(f, 2, 2)
	}
	switch o2 := args[0].(type) {
	case nil:
		return slip.List{args[1]}
	case slip.List:
		return append(o2, args[1])
	default:
		return slip.Cons{o2, args[1]}
	}
}
