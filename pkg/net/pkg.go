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
		PreSet:    slip.DefaultPreSet,
		Vars:      map[string]*slip.VarVal{},
	}
)

func init() {
	Pkg.Initialize()
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*net*", &Pkg)
}
