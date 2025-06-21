// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"os"
	"plugin"
	"strings"

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

// Require represents the require function.
type Require struct {
	slip.Function
}

// Call the function with the arguments provided.
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
			paths = append(paths, expandPath(string(ta)))
		case slip.List:
			for _, a2 := range ta {
				if str, ok := a2.(slip.String); ok {
					paths = append(paths, expandPath(string(str)))
				} else {
					slip.PanicType("path members", a2, "string")
				}
			}
		default:
			slip.PanicType("name", ta, "string", "symbol")
		}
	}
	if len(paths) == 0 {
		lp := s.Get("*package-load-path*")
		switch tp := lp.(type) {
		case nil:
		case slip.String:
			paths = append(paths, expandPath(string(tp)))
		case slip.List:
			for _, a2 := range tp {
				if str, ok := a2.(slip.String); ok {
					paths = append(paths, expandPath(string(str)))
				} else {
					slip.PanicType("*package-load-path* members", a2, "string")
				}
			}
		default:
			slip.PanicType("*package-load-path*", lp, "string", "list of strings")
		}
	}
	defer func() { slip.CurrentPackageLoadPath = "" }()

	for _, path := range paths {
		filepath := fmt.Sprintf("%s/%s.so", path, name)
		if _, err := os.Stat(filepath); err == nil {
			slip.CurrentPackageLoadPath = filepath
			if _, err = plugin.Open(filepath); err != nil {
				slip.NewPanic("plugin %s open failed. %s", filepath, err)
			}
			return slip.Novalue
		}
		if strings.HasSuffix(strings.ToLower(name), ".lisp") {
			filepath = fmt.Sprintf("%s/%s", path, name)
		} else {
			filepath = fmt.Sprintf("%s/%s.lisp", path, name)
		}
		if _, err := os.Stat(filepath); err == nil {
			slip.CurrentPackageLoadPath = filepath
			loadLispFile(s, filepath)
			return slip.Novalue
		}
	}
	panic(slip.NewError("Failed to find %s in any of the load paths.", name))
}

func expandPath(path string) string {
	if 0 < len(path) && path[0] == '~' {
		home := os.Getenv("HOME")
		path = home + path[1:]
	}
	return path
}

func loadLispFile(s *slip.Scope, path string) {
	buf, err := os.ReadFile(path)
	if err != nil {
		slip.NewPanic("loading %s: %s", path, err)
	}
	defer func() {
		s.Set(slip.Symbol("*load-pathname*"), nil)
		s.Set(slip.Symbol("*load-truename*"), nil)
	}()
	s.Set(slip.Symbol("*load-pathname*"), slip.String(path))
	s.Set(slip.Symbol("*load-truename*"), slip.String(path))
	code := slip.Read(buf, s)
	code.Compile()
	code.Eval(s, nil)
}
