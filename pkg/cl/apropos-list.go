// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"sort"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := AproposList{Function: slip.Function{Name: "apropos-list", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "apropos-list",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string|symbol",
					Text: "A pattern to search for across symbols and functions.",
				},
				{Name: "&optional"},
				{
					Name: "package",
					Type: "string|symbol",
					Text: "A package to limit the search to.",
				},
			},
			Return: "list",
			Text:   `Return all symbols that contain the provided _string_ and returns the matchs in a list.`,
			Examples: []string{
				`(apropos-list "terpri") => (terpri)`,
			},
		}, &slip.CLPkg)
}

// AproposList represents the aproposList function.
type AproposList struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *AproposList) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	var pat string
	switch ta := args[0].(type) {
	case slip.Symbol:
		pat = strings.ToLower(string(ta))
	case slip.String:
		pat = strings.ToLower(string(ta))
	default:
		slip.PanicType("string", ta, "string", "symbol")
	}
	var pkg *slip.Package
	if 1 < len(args) {
		var name string
		switch ta := args[1].(type) {
		case slip.Symbol:
			name = string(ta)
		case slip.String:
			name = string(ta)
		default:
			slip.PanicType("package", ta, "string", "symbol")
		}
		if pkg = slip.FindPackage(name); pkg == nil {
			slip.NewPanic("Package %s not found.", name)
		}
	}
	var list slip.List
	if pkg != nil {
		for k := range pkg.Vars {
			if !strings.Contains(k, pat) {
				continue
			}
			list = append(list, slip.Symbol(k))
		}
		pkg.EachFuncName(func(name string) {
			if strings.Contains(name, pat) {
				list = append(list, slip.Symbol(name))
			}
		})
	} else {
		for _, pn := range slip.PackageNames() {
			pkg := slip.FindPackage(string(pn.(slip.String)))
			for k, vv := range pkg.Vars {
				if vv.Pkg != pkg || !strings.Contains(k, pat) {
					continue
				}
				list = append(list, slip.Symbol(k))
			}
			pkg.EachFuncInfo(func(fi *slip.FuncInfo) {
				if fi.Pkg == pkg && strings.Contains(fi.Name, pat) {
					list = append(list, slip.Symbol(fi.Name))
				}
			})
		}
	}
	sort.Slice(list, func(i, j int) bool { return string(list[i].(slip.Symbol)) < string(list[j].(slip.Symbol)) })

	return list
}
