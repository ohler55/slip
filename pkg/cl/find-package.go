// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FindPackage{Function: slip.Function{Name: "find-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "find-package",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string|symbol",
					Text: "The _string_ to be searched for.",
				},
			},
			Return: "package",
			Text: `__find-package__ Finds a _package_ matching _name_. This function deviates from
the common lisp specification isn that is it not case sensitive.`,
			Examples: []string{
				`(find-package "Cl") => #<package "common-lisp">`,
			},
		}, &slip.CLPkg)
}

// FindPackage represents the findPackage function.
type FindPackage struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *FindPackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	switch ta := args[0].(type) {
	case slip.Symbol:
		if p := slip.FindPackage(string(ta)); p != nil {
			return p
		}
	case slip.String:
		if p := slip.FindPackage(string(ta)); p != nil {
			return p
		}
	default:
		slip.PanicType("name", args[0], "string", "symbol")
	}
	return
}
