// Copyright (c) 2022, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

func defFindClass() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindClass{Function: slip.Function{Name: "find-class", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-class",
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
			Text: `__find-class__ returns the _flavor_ with the _name_ specified or _nil_ if not found.
If _errorp_ is true then a panic is raised. This is an alias for __find-flavor__.`,
			Examples: []string{
				"(find-class 'vanilla-flavor) => #<flavor vanilla-flavor>",
			},
		}, &Pkg)
}

// FindClass represents the find-class function.
type FindClass struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FindClass) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	sym, ok := args[0].(slip.Symbol)
	if !ok {
		slip.TypePanic(s, depth, "name", args[0], "symbol")
	}
	if c := slip.FindClass(string(sym)); c != nil {
		return c
	}
	if 1 < len(args) && args[1] != nil {
		slip.PanicClassNotFound(sym, "%s is not a defined flavor.", sym)
	}
	return nil
}
