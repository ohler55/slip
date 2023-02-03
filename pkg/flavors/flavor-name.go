// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"

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
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	pos := len(args) - 1
	var cf *Flavor
	switch ta := args[pos].(type) {
	case slip.Symbol:
		cf = allFlavors[string(ta)]
	case *Flavor:
		cf = ta
	default:
		slip.PanicType("flavor argument to flavor-name", ta, "flavor")
	}
	if cf == nil {
		panic(fmt.Sprintf("%s is not a defined flavor.", args[pos]))
	}
	return slip.Symbol(cf.name)
}
