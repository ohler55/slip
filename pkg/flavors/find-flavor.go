// Copyright (c) 2022, Peter Ohler, All rights reserved.

package flavors

import (
	"fmt"

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
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	pos := len(args) - 1
	var cf *Flavor
	sym, ok := args[pos].(slip.Symbol)
	if !ok {
		slip.PanicType("name", args[pos], "symbol")
	}
	cf = allFlavors[string(sym)]
	if cf != nil {
		return cf
	}
	if 1 < len(args) && args[0] != nil {
		panic(fmt.Sprintf("%s is not a defined flavor.", args[pos]))
	}
	return nil
}
