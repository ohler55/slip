// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the net package.
	Pkg = slip.Package{
		Name:      "net",
		Nicknames: []string{"networking", "network"},
		Doc:       "Home of symbols defined for the net (networking) package.",
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
		PreSet:    slip.DefaultPreSet,
		Vars:      map[string]*slip.VarVal{},
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*net*", &Pkg)
}
