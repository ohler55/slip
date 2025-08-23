// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FlavorName{Function: slip.Function{Name: "flavor-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "flavor-name",
			Args: []*slip.DocArg{
				{
					Name: "flavor",
					Type: "flavor",
					Text: "The name of the flavor.",
				},
			},
			Text: `__flavor-name__ returns the name of a flavor.`,
			Examples: []string{
				"(flavor-name 'vanilla-flavor) => vanilla-flavor",
			},
		}, &Pkg)
}

// FlavorName represents the flavor-name function.
type FlavorName struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FlavorName) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var cf *Flavor
	switch ta := args[0].(type) {
	case slip.Symbol:
		if cf = allFlavors[string(ta)]; cf == nil {
			slip.ClassNotFoundPanic(s, depth, ta, "%s is not a defined flavor.", ta)
		}
	case *Flavor:
		cf = ta
	default:
		slip.TypePanic(s, depth, "flavor argument to flavor-name", ta, "flavor")
	}
	return slip.Symbol(cf.name)
}
