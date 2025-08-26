// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RenamePackage{Function: slip.Function{Name: "rename-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rename-package",
			Args: []*slip.DocArg{
				{
					Name: "package",
					Type: "package",
					Text: "The package to rename.",
				},
				{
					Name: "new-name",
					Type: "string|symbol",
					Text: "The new name of the package.",
				},
				{Name: "&optional"},
				{
					Name: "new-nicknames",
					Type: "list of strings",
					Text: "The new nicknames of the package.",
				},
			},
			Return: "package",
			Text: `__rename-package__ renames a _package_ with the _new-name_ and sets the package
nicknames to _new-nicknames_.`,
			Examples: []string{
				`(setq pkg (make-package 'new-pack-1))`,
				`(rename-package pkg 'quux) => #<package quux>`,
			},
		}, &slip.CLPkg)
}

// RenamePackage represents the renamePackage function.
type RenamePackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RenamePackage) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 2, 3)
	pkg := slip.PackageFromArg(args[0])
	if pkg == nil {
		slip.PackagePanic(s, depth, nil, "Package %s does not exist.", args[0])
	}
	name := slip.MustBeString(args[1], "new-name")
	if slip.FindPackage(name) != nil {
		slip.ErrorPanic(s, depth, "Package %s already exists.", name)
	}
	if 2 < len(args) {
		var nicknames []string
		list, ok := args[2].(slip.List)
		if !ok {
			slip.TypePanic(s, depth, "nicknames", args[2], "list")
		}
		for _, v := range list {
			nn := slip.MustBeString(v, "nickname")
			if slip.FindPackage(nn) != nil {
				slip.ErrorPanic(s, depth, "Package %s already exists.", nn)
			}
			nicknames = append(nicknames, nn)
		}
		pkg.Nicknames = nicknames
	}
	pkg.Name = name

	return pkg
}
