// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LockPackage{Function: slip.Function{Name: "lock-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "lock-package",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package designator",
					Text: "Package to lock.",
				},
			},
			Return: "boolean",
			Text:   `__lock-package__ locks _package_.`,
			Examples: []string{
				`(lock-package 'common-lisp-user) => nil`,
				`(package-locked-p 'common-lisp-user) => t`,
			},
		}, &Pkg)
}

// LockPackage represents the lock-package function.
type LockPackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *LockPackage) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg := slip.PackageFromArg(args[0])
	pkg.Locked = true

	return nil
}
