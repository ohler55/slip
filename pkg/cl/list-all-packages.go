// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ListAllPackages{Function: slip.Function{Name: "list-all-packages", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name:   "list-all-packages",
			Args:   []*slip.DocArg{},
			Return: "list",
			Text:   `Return a list of all the packages.`,
			Examples: []string{
				`(listAllPackages) =>  ();; `,
			},
		}, &slip.CLPkg)
}

// ListAllPackages represents the listAllPackages function.
type ListAllPackages struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ListAllPackages) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 0, 0)
	var pkgs slip.List
	for _, str := range slip.PackageNames() {
		pkgs = append(pkgs, slip.FindPackage(string(str.(slip.String))))
	}
	return pkgs
}
