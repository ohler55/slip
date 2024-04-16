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
			Text: `__rename-package__ Renames a _package_ with the _new-name_ and sets the package
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
	slip.ArgCountCheck(f, args, 2, 3)
	var pkg *slip.Package
	switch tv := args[0].(type) {
	case slip.Symbol:
		pkg = slip.FindPackage(string(tv))
	case slip.String:
		pkg = slip.FindPackage(string(tv))
	case *slip.Package:
		pkg = tv
	default:
		slip.PanicType("use", tv, "symbol", "string", "package")
	}
	if pkg == nil {
		slip.NewPanic("Package %s does not exist.", args[0])
	}
	name := slip.MustBeString(args[1], "new-name")
	if slip.FindPackage(name) != nil {
		slip.NewPanic("Package %s already exists.", name)
	}
	if 2 < len(args) {
		var nicknames []string
		list, ok := args[2].(slip.List)
		if !ok {
			slip.PanicType("nicknames", args[2], "list")
		}
		for _, v := range list {
			nn := slip.MustBeString(v, "nickname")
			if slip.FindPackage(nn) != nil {
				slip.NewPanic("Package %s already exists.", nn)
			}
			nicknames = append(nicknames, nn)
		}
		pkg.Nicknames = nicknames
	}
	pkg.Name = name

	return pkg
}
