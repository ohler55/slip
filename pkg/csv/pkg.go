// Copyright (c) 2023, Peter Ohler, All rights reserved.

package csv

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the csv package.
	Pkg = slip.Package{
		Name:      "csv",
		Nicknames: []string{"csv"},
		Doc:       "Home of symbols defined for the CSV functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*csv*": {Val: &Pkg, Doc: Pkg.Doc},
		},
		&Read{},
	)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
