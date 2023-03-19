// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"os"
	"plugin"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Require{Function: slip.Function{Name: "require", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "require",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string|symbol",
					Text: "The name of a package to load.",
				},
				{Name: "&optional"},
				{
					Name: "path",
					Type: "string|list",
					Text: "The filepath or a list of filepaths to look for the package in.",
				},
			},
			Return: "",
			Text: `__require__ attempts to load the package __name__ from the provided __path__
if provided. If __path__ is __nil__ then __*package-load-path*__ is used.`,
			Examples: []string{
				"(require 'stuff) ;; loads stuff.so",
			},
		}, &slip.CLPkg)
}

// Require represents the != function.
type Require struct {
	slip.Function
}

// Call the the function with the arguments provided.
func (f *Require) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	if len(args) < 1 || 2 < len(args) {
		slip.PanicArgCount(f, 1, 2)
	}
	var name string
	switch ta := args[0].(type) {
	case slip.String:
		name = string(ta)
	case slip.Symbol:
		name = string(ta)
	default:
		slip.PanicType("name", ta, "string", "symbol")
	}
	var paths []string

	if 1 < len(args) {
		switch ta := args[1].(type) {
		case slip.String:
			paths = append(paths, string(ta))
		case slip.List:
			for _, a2 := range ta {
				if str, ok := a2.(slip.String); ok {
					paths = append(paths, string(str))
				} else {
					slip.PanicType("path members", a2, "string")
				}
			}
		default:
			slip.PanicType("name", ta, "string", "symbol")
		}
	}
	if len(paths) == 0 {
		lp := slip.CurrentPackage.JustGet("*package-load-path*")
		switch tp := lp.(type) {
		case nil:
		case slip.String:
			paths = append(paths, string(tp))
		case slip.List:
			for _, a2 := range tp {
				if str, ok := a2.(slip.String); ok {
					paths = append(paths, string(str))
				} else {
					slip.PanicType("*package-load-path* members", a2, "string")
				}
			}
		default:
			slip.PanicType("*package-load-path*", lp, "string", "list of strings")
		}
	}
	for _, path := range paths {
		filepath := fmt.Sprintf("%s/%s.so", path, name)
		if _, err := os.Stat(filepath); err == nil {
			if _, err = plugin.Open(filepath); err != nil {
				panic(err)
			}
			return slip.Novalue
		}
	}
	panic(fmt.Sprintf("Failed to find %s in any of the load paths.", name))
}
