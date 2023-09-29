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
		Vars:      map[string]*slip.VarVal{},
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*csv*", &Pkg)
}
