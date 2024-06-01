// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DeletePackage{Function: slip.Function{Name: "delete-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "delete-package",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package designator",
					Text: "The package to delete.",
				},
			},
			Return: "boolean",
			Text: `__delete-package__ deletes a _package_ and return _t_ on success of _nil_ if
the package can not be deleted.`,
			Examples: []string{
				`(setq pkg (make-package 'new-pack-1))`,
				`(delete-package pkg) => t`,
				`(delete-package 'bag) => nil ;; package is locked`,
			},
		}, &slip.CLPkg)
}

// DeletePackage represents the deletePackage function.
type DeletePackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DeletePackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg := slip.PackageFromArg(args[0])
	if pkg == nil {
		slip.PanicPackage(nil, "Package %s does not exist.", args[0])
	}
	if pkg.Locked {
		slip.PanicPackage(pkg, "Package %s is locked and can not be deleted.", pkg)
	}
	if 0 < len(pkg.Users) {
		slip.PanicPackage(pkg, "Package %s is still in use and can not be deleted.", pkg)
	}
	if len(pkg.Name) == 0 {
		return nil
	}
	slip.RemovePackage(pkg)

	return slip.True
}
