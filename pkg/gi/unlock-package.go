// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UnlockPackage{Function: slip.Function{Name: "unlock-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unlock-package",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package designator",
					Text: "Package to unlock.",
				},
			},
			Return: "boolean",
			Text:   `__unlock-package__ unlocks _package_.`,
			Examples: []string{
				`(unlock-package 'my-package) => nil`,
				`(package-unlocked-p 'my-package) => nil`,
			},
		}, &Pkg)
}

// UnlockPackage represents the unlock-package function.
type UnlockPackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UnlockPackage) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	pkg := slip.PackageFromArg(args[0])
	pkg.Locked = false

	return nil
}
