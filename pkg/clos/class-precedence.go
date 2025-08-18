// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defClassPrecedence() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClassPrecedence{Function: slip.Function{Name: "class-precedence", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "class-precedence",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "class|flavor",
					Text: "The name of the class or flavor.",
				},
			},
			Return: "list",
			Text:   `__class-precedence__ returns the precedence of a class or flavor.`,
			Examples: []string{
				"(class-precedence 'bag-flavor) => (bag-flavor vanilla-flavor)",
			},
		}, &Pkg)
}

// ClassPrecedence represents the class-precedence function.
type ClassPrecedence struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassPrecedence) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	c := classFromArg0(f, s, args)
top:
	switch tc := c.(type) {
	case *StandardClass:
		pa := make(slip.List, len(tc.precedence))
		for i, name := range tc.precedence {
			pa[i] = name
		}
		result = pa
	case *ConditionClass:
		c = &tc.StandardClass
		goto top
	case *flavors.Flavor:
		inh := tc.Precedence
		pa := make(slip.List, len(inh))
		for i, f := range inh {
			pa[i] = f
		}
		result = pa
	case *BuiltInClass:
		pa := make(slip.List, len(tc.precedence))
		for i, name := range tc.precedence {
			pa[i] = name
		}
		result = pa
	default:
		h := tc.Hierarchy()
		pa := make(slip.List, len(h))
		for i, name := range h {
			pa[i] = name
		}
		result = pa
	}
	return
}
