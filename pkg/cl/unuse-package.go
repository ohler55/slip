// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UnusePackage{Function: slip.Function{Name: "unuse-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unuse-package",
			Args: []*slip.DocArg{
				{
					Name: "package-to-unuse",
					Type: "package designator",
					Text: "The package to unuse.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "package designator",
					Text: "The package to update.",
				},
			},
			Return: "boolean",
			Text:   `__unuse-package__ updates _package_ to unuse _package-to-unuse_.`,
			Examples: []string{
				`(setq p1 (make-package 'pack-1)`,
				`      p2 (make-package 'pack-2))`,
				`(unuse-package p1 p2) => t`,
			},
		}, &slip.CLPkg)
}

// UnusePackage represents the unusePackage function.
type UnusePackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UnusePackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	pkg := slip.CurrentPackage
	pto := packageFromArg(args[0], "package-to-unuse")
	if pto == nil {
		slip.PanicPackage(nil, "Package %s does not exist.", args[0])
	}
	if 1 < len(args) {
		if pkg = packageFromArg(args[1], "package"); pkg == nil {
			slip.PanicPackage(nil, "Package %s does not exist.", args[0])
		}
	}
	pkg.Unuse(pto)

	return slip.True
}
