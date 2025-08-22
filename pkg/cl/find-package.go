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

// Call the function with the arguments provided.
func (f *FindPackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	var p *slip.Package
	switch ta := args[0].(type) {
	case slip.Symbol:
		if 0 < len(ta) && ta[0] == ':' {
			p = slip.FindPackage(string(ta[1:]))
		} else {
			p = slip.FindPackage(string(ta))
		}
	case slip.String:
		p = slip.FindPackage(string(ta))
	default:
		slip.TypePanic(s, depth, "name", args[0], "string", "symbol")
	}
	if p != nil {
		result = p
	}
	return
}
