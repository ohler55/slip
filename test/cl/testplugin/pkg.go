// Copyright (c) 2022, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Bag package.
	Pkg = slip.Package{
		Name:      "testplugin",
		Nicknames: []string{},
		Doc:       "Plugin test package.",
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
		Vars:      map[string]*slip.VarVal{},
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*testplugin*", &Pkg)
}
