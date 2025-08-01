// Copyright (c) 2025, Peter Ohler, All rights reserved.

package generic

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Class package.
	Pkg = slip.Package{
		Name:      "generic",
		Nicknames: []string{"generic"},
		Doc:       "Home of symbols defined for the generic functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*generic*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
		},
		&Defmethod{},
	)
	defDefmethod()
	defNoApplicableMethod()
	defSlotMissing()
	defSlotUnbound()
	defDefgeneric()
	defCallNextMethod()

	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
