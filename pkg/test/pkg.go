// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Test package.
	Pkg = slip.Package{
		Name:      "test",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the test package.",
		PreSet:    slip.DefaultPreSet,
		Vars: map[string]*slip.VarVal{
			"*current-test*": {
				Val: nil,
				Doc: "is bound to the current test if there is a test running.",
			},
		},
	}
)

func init() {
	Pkg.Initialize()
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*test*", &Pkg)
	for _, vv := range Pkg.Vars {
		vv.Pkg = &Pkg
	}
}
