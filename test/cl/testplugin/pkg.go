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
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(nil)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*testplugin*", &Pkg)
}
