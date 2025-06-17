// Copyright (c) 2025, Peter Ohler, All rights reserved.

package main

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg a app test package.
	Pkg = slip.Package{
		Name:      "appplugin",
		Nicknames: []string{},
		Doc:       "Plugin app test package.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(nil)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*appplugin*", &Pkg)
}
