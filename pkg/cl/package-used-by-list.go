// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PackageUsedByList{Function: slip.Function{Name: "package-used-by-list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "package-used-by-list",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package",
					Text: "The package to get the name from.",
				},
			},
			Return: "string",
			Text:   `__package-used-by-list__ returns the name of the _package_.`,
			Examples: []string{
				`(package-used-by-list *gi*) => (#<package common-lisp-user>)`,
			},
		}, &slip.CLPkg)
}

// PackageUsedByList represents the package-used-by-list function.
type PackageUsedByList struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PackageUsedByList) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg, ok := args[0].(*slip.Package)
	if !ok {
		slip.PanicType("package", args[0], "package")
	}
	ul := make(slip.List, len(pkg.Users))
	for i, u := range pkg.Users {
		ul[i] = u
	}
	return ul
}
