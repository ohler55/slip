// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := PackageNicknames{Function: slip.Function{Name: "package-nicknames", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "package-nicknames",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package",
					Text: "The package to get the name from.",
				},
			},
			Return: "list of string",
			Text:   `__package-nicknames__ returns the nicknames of the _package_.`,
			Examples: []string{
				`(package-nicknames :cl) => ("cl")`,
			},
		}, &slip.CLPkg)
}

// PackageNicknames represents the package-nicknames function.
type PackageNicknames struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *PackageNicknames) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	pkg := slip.PackageFromArg(args[0])
	nn := make(slip.List, len(pkg.Nicknames))
	for i, str := range pkg.Nicknames {
		nn[i] = slip.String(str)
	}
	return nn
}
