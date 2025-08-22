// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindFlavor{Function: slip.Function{Name: "find-flavor", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-flavor",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "symbol",
					Text: "The name of a flavor.",
				},
				{Name: "&optional"},
				{
					Name: "errorp",
					Type: "boolean",
					Text: "If true a panic is raised if the flavor is not found.",
				},
			},
			Text: `__find-flavor__ returns the _flavor_ with the _name_ specified or _nil_ if not found.
If _errorp_ is true then a panic is raised.`,
			Examples: []string{
				"(find-flavor 'vanilla-flavor) => #<flavor vanilla-flavor>",
			},
		}, &Pkg)
}

// FindFlavor represents the findFlavor function.
type FindFlavor struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FindFlavor) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	var cf *Flavor
	name := slip.MustBeString(args[0], "name")
	if cf = Find(name); cf != nil {
		return cf
	}
	if 1 < len(args) && args[1] != nil {
		slip.PanicClassNotFound(args[0], "%s is not a defined flavor.", name)
	}
	return nil
}
