// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PackageName{Function: slip.Function{Name: "package-name", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "package-name",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package",
					Text: "The package to get the name from.",
				},
			},
			Return: "string",
			Text:   `__package-name__ returns the name of the _package_.`,
			Examples: []string{
				`(package-name *cl*) => "common-lisp"`,
			},
		}, &slip.CLPkg)
}

// PackageName represents the package-name function.
type PackageName struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PackageName) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg, ok := args[0].(*slip.Package)
	if !ok {
		slip.PanicType("package", args[0], "package")
	}
	return slip.String(pkg.Name)
}
