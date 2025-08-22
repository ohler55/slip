// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PackageLockedp{Function: slip.Function{Name: "package-locked-p", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "package-locked-p",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package designator",
					Text: "Package to check the lock status of.",
				},
			},
			Return: "boolean",
			Text:   `__package-locked-p__ returns true if _package_ is locked otherwise nil.`,
			Examples: []string{
				`(package-locked-p *common-lisp*) => t`,
				`(package-locked-p 'common-lisp-user) => nil`,
			},
		}, &Pkg)
}

// PackageLockedp represents the package-locked-p function.
type PackageLockedp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PackageLockedp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	pkg := slip.PackageFromArg(args[0])
	if pkg.Locked {
		return slip.True
	}
	return nil
}
