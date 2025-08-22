// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Undefflavor{Function: slip.Function{Name: "undefflavor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "undefflavor",
			Args: []*slip.DocArg{
				{
					Name: "flavor",
					Type: "symbol",
					Text: "The name of the flavor being undefined.",
				},
			},
			Return: "object",
			Text: `__undeffalvor__ removes the _flavor_ and all flavors that inherit the _flavor_.
Instances of the removed flavors are still valid but no new instance can be created.`,
			Examples: []string{
				"(undefflavor 'strawberry) => nil",
			},
		}, &Pkg)
}

// Undefflavor represents the undefflavor function.
type Undefflavor struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Undefflavor) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var cf *Flavor
	switch ta := args[0].(type) {
	case slip.Symbol:
		if cf = allFlavors[string(ta)]; cf == nil {
			slip.PanicClassNotFound(ta, "%s is not a defined flavor.", ta)
		}
	case *Flavor:
		cf = ta
	default:
		slip.TypePanic(s, depth, "flavor argument to undefflavor", ta, "symbol", "flavor")
	}
	f.removeFlavor(cf)

	return nil
}

func (f *Undefflavor) removeFlavor(cf *Flavor) {
	delete(allFlavors, cf.name)
	Pkg.Remove(cf.name)
	for _, f2 := range allFlavors {
		if f2.inheritsFlavor(cf.name) {
			f.removeFlavor(f2)
		}
	}
}
