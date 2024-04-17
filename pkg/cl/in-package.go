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
					Name: "package",
					Type: "package designator",
					Text: "The _string_ to be searched for.",
				},
			},
			Return: "package",
			Text:   `__in-package__ finds a _package_ matching _package_ and sets _*package*_ to the found package.`,
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
func (f *InPackage) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg := packageFromArg(args[0], "package")
	if pkg == nil {
		slip.PanicPackage(nil, "The name %q does not designate any package.", pkg)
	}
	slip.CLPkg.Set("*package*", pkg)

	return pkg
}
