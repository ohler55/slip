// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UsePackage{Function: slip.Function{Name: "use-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "use-package",
			Args: []*slip.DocArg{
				{
					Name: "package-to-use",
					Type: "package designator",
					Text: "The package to use.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "package designator",
					Text: "The package to update.",
				},
			},
			Return: "boolean",
			Text:   `__use-package__ updates _package_ to use _package-to-use_.`,
			Examples: []string{
				`(setq p1 (make-package 'pack-1)`,
				`      p2 (make-package 'pack-2))`,
				`(use-package p1 p2) => t`,
			},
		}, &slip.CLPkg)
}

// UsePackage represents the use-package function.
type UsePackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UsePackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	pkg := slip.CurrentPackage
	pto := slip.PackageFromArg(args[0])
	if pto == nil {
		slip.PanicPackage(nil, "Package %s does not exist.", args[0])
	}
	if 1 < len(args) {
		if pkg = slip.PackageFromArg(args[1]); pkg == nil {
			slip.PanicPackage(nil, "Package %s does not exist.", args[0])
		}
	}
	pkg.Use(pto)

	return slip.True
}
