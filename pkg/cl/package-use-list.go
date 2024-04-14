// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PackageUseList{Function: slip.Function{Name: "package-use-list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "package-use-list",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package",
					Text: "The package to get the name from.",
				},
			},
			Return: "string",
			Text:   `__package-use-list__ returns the name of the _package_.`,
			Examples: []string{
				`(package-use-list *common-lisp-user*) => (#<package keyword> #<package common-lisp> ...)`,
			},
		}, &slip.CLPkg)
}

// PackageUseList represents the package-use-list function.
type PackageUseList struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PackageUseList) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg, ok := args[0].(*slip.Package)
	if !ok {
		slip.PanicType("package", args[0], "package")
	}
	ul := make(slip.List, len(pkg.Uses))
	for i, u := range pkg.Uses {
		ul[i] = u
	}
	return ul
}
