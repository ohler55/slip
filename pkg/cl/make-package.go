// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakePackage{Function: slip.Function{Name: "make-package", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-package",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string|symbol",
					Text: "The name of the new package being created.",
				},
				{Name: "&key"},
				{
					Name: "nicknames",
					Type: "list of strings",
					Text: "The nicknames of the new package.",
				},
				{
					Name: "use",
					Type: "list of packages",
					Text: "The packages the new package should use.",
				},
			},
			Return: "package",
			Text:   `__make-package__ makes a new _package_ with the _name_, _nicknames_, and _use_.`,
			Examples: []string{
				`(make-package 'quux) => #<package quux>`,
			},
		}, &slip.CLPkg)
}

// MakePackage represents the make-package function.
type MakePackage struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakePackage) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 5)
	name := slip.MustBeString(args[0], "name")
	if slip.FindPackage(name) != nil {
		slip.NewPanic("Package %s already exists.", name)
	}
	var nicknames []string
	rest := args[1:]
	if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":nicknames")); has {
		list, ok := value.(slip.List)
		if !ok {
			slip.PanicType("nicknames", value, "list")
		}
		for _, v := range list {
			nn := slip.MustBeString(v, "nickname")
			if slip.FindPackage(nn) != nil {
				slip.NewPanic("Package %s already exists.", nn)
			}
			nicknames = append(nicknames, nn)
		}
	}
	var pkgs []*slip.Package
	if value, has := slip.GetArgsKeyValue(rest, slip.Symbol(":use")); has {
		list, ok := value.(slip.List)
		if !ok && value != nil {
			slip.PanicType("use", value, "list")
		}
		for _, v := range list {
			var p *slip.Package
			switch tv := v.(type) {
			case slip.Symbol:
				p = slip.FindPackage(string(tv))
			case slip.String:
				p = slip.FindPackage(string(tv))
			case *slip.Package:
				p = tv
			default:
				slip.PanicType("use", tv, "symbol", "string", "package")
			}
			if p == nil {
				slip.NewPanic("Package %s does not exist.", v)
			}
			pkgs = append(pkgs, p)
		}
	}
	pkg := slip.DefPackage(name, nicknames, "")
	for _, p := range pkgs {
		pkg.Use(p)
	}
	return pkg
}
