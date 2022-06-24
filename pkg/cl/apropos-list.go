// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
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
			Text: `Search all symbols for a symbols that contains the provided string or symbol.
The matches are printed to *standard-output* along with the package they are from and what the
symbol is associated with.`,
			Examples: []string{
				`(aproposList "terpri") => (terpri)`,
			},
		}, &slip.CLPkg)
}

// AproposList represents the aproposList function.
type AproposList struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *AproposList) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	var pat string
	switch ta := args[len(args)-1].(type) {
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
		switch ta := args[0].(type) {
		case slip.Symbol:
			name = string(ta)
		case slip.String:
			name = string(ta)
		default:
			slip.PanicType("package", ta, "string", "symbol")
		}
		if pkg = slip.FindPackage(name); pkg == nil {
			panic(fmt.Sprintf("Package %s not found.", name))
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
		for k := range pkg.Funcs {
			if !strings.Contains(k, pat) {
				continue
			}
			list = append(list, slip.Symbol(k))
		}
	} else {
		for _, pn := range slip.PackageNames() {
			pkg := slip.FindPackage(string(pn.(slip.String)))
			for k, vv := range pkg.Vars {
				if vv.Pkg != pkg || !strings.Contains(k, pat) {
					continue
				}
				list = append(list, slip.Symbol(k))
			}
			for k, fi := range pkg.Funcs {
				if fi.Pkg != pkg || !strings.Contains(k, pat) {
					continue
				}
				list = append(list, slip.Symbol(k))
			}
		}
	}
	sort.Slice(list, func(i, j int) bool { return string(list[j].(slip.Symbol)) < string(list[i].(slip.Symbol)) })

	return list
}
