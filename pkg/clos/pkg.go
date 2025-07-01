// Copyright (c) 2023, Peter Ohler, All rights reserved.

package clos

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Class package.
	Pkg = slip.Package{
		Name:      "clos",
		Nicknames: []string{"clos"},
		Doc:       "Home of symbols defined for the CLOS functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*clos*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
		},
		&ClassName{},
	)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
