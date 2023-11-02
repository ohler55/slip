// Copyright (c) 2023, Peter Ohler, All rights reserved.

package parquet

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the parquet package.
	Pkg = slip.Package{
		Name:      "parquet",
		Nicknames: []string{"parquet"},
		Doc:       "Home of symbols defined for the parquet functions, variables, and constants.",
		Vars:      map[string]*slip.VarVal{},
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*parquet*", &Pkg)
}
