// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := InPackage{Function: slip.Function{Name: "in-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "in-package",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string|symbol",
					Text: "The _string_ to be searched for.",
				},
			},
			Return: "package",
			Text:   `__in-package__ finds a _package_ matching _name_ and sets _*package*_ to the found package.`,
			Examples: []string{
				`(in-package "Cl") => #<package "common-lisp">`,
			},
		}, &slip.CLPkg)
}

// InPackage represents the inPackage function.
type InPackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *InPackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	if len(args) != 1 {
		slip.PanicArgCount(f, 1, 1)
	}
	var name string
	switch ta := args[0].(type) {
	case slip.Symbol:
		name = string(ta)
	case slip.String:
		name = string(ta)
	default:
		slip.PanicType("name", args[0], "string", "symbol")
	}
	p := slip.FindPackage(name)
	if p == nil {
		slip.PanicPackage(nil, "The name %q does not designate any package.", name)
	}
	slip.CLPkg.Set("*package*", p)

	return p
}
