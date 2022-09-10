// Copyright (c) 2022, Peter Ohler, All rights reserved.

package bag

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Bag package.
	Pkg = slip.Package{
		Name:      "bag",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the bag package.",
		Vars:      map[string]*slip.VarVal{},
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*bag*", &Pkg)
}
