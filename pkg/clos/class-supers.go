// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defClassSupers() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClassSupers{Function: slip.Function{Name: "class-supers", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "class-supers",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "class|flavor",
					Text: "The name of the class or flavor.",
				},
			},
			Return: "list",
			Text:   `__class-supers__ returns a list of the names of the direct supers of a class or flavor.`,
			Examples: []string{
				"(class-supers 'bag-flavor) => (vanilla-flavor)",
			},
		}, &Pkg)
}

// ClassSupers represents the class-supers function.
type ClassSupers struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassSupers) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	c := classFromArg0(f, s, args, depth)
top:
	switch tc := c.(type) {
	case *StandardClass:
		supers := make(slip.List, len(tc.supers))
		for i, super := range tc.supers {
			supers[i] = super
		}
		result = supers
	case *ConditionClass:
		c = &tc.StandardClass
		goto top
	case *flavors.Flavor:
		inh := tc.InheritsList()
		supers := make(slip.List, len(inh))
		for i, f := range inh {
			supers[i] = slip.Symbol(f.Name())
		}
		result = supers
	case *BuiltInClass:
		if tc.inherit != nil {
			result = slip.List{slip.Symbol(tc.inherit.Name())}
		}
	}
	return
}
