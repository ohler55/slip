// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ClassName{Function: slip.Function{Name: "class-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "class-name",
			Args: []*slip.DocArg{
				{
					Name: "class",
					Type: "flavor",
					Text: "The name of the flavor.",
				},
			},
			Text: `__class-name__ returns the name of a flavor.`,
			Examples: []string{
				"(class-name 'vanilla-flavor) => vanilla-flavor",
			},
		}, &Pkg)
}

// ClassName represents the class-name function.
type ClassName struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *ClassName) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	pos := len(args) - 1
	var cf *Flavor
	switch ta := args[pos].(type) {
	case slip.Symbol:
		if cf = allFlavors[string(ta)]; cf == nil {
			slip.PanicClassNotFound(ta, "%s is not a defined flavor.", ta)
		}
	case *Flavor:
		cf = ta
	default:
		slip.PanicType("class argument to class-name", ta, "flavor")
	}
	return slip.Symbol(cf.name)
}
