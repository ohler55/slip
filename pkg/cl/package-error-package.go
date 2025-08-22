// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

const packageSymbol = slip.Symbol("package")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PackageErrorPackage{Function: slip.Function{Name: "package-error-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "package-error-package",
			Args: []*slip.DocArg{
				{
					Name: "condition",
					Type: "package-error",
					Text: "The package-error or subclass of package-error to get the package of.",
				},
			},
			Return: "object",
			Text: `__package-error-package__ returns the value of the _package_ slot in the _condition_
which must be of package _package-error_ or inherit from _package-error_.`,
			Examples: []string{
				`(package-error-package (make-condition 'package-error :package *package*)) =>`,
				`  #<package common-lisp-user>)`,
			},
		}, &slip.CLPkg)
}

// PackageErrorPackage represents the package-error-package function.
type PackageErrorPackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PackageErrorPackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	if ci, ok := args[0].(slip.Instance); !ok || !ci.IsA("package-error") {
		slip.TypePanic(s, depth, "package-error", args[0], "package-error")
	} else {
		result, _ = ci.SlotValue(packageSymbol)
	}
	return
}
