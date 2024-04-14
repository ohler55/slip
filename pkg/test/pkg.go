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
	}
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{
		"*current-test*": {
			Val: nil,
			Doc: "is bound to the current test if there is a test running.",
		},
		"*test*": {Val: &Pkg, Doc: Pkg.Doc},
	})
	_ = SuiteFlavor()
	_ = TestFlavor()

	Pkg.Initialize(nil, &AssertEqual{})
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
