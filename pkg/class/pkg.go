// Copyright (c) 2023, Peter Ohler, All rights reserved.

package class

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Class package.
	Pkg = slip.Package{
		Name:      "class",
		Nicknames: []string{"class"},
		Doc:       "Home of symbols defined for the classfunctions, variables, and constants.",
		Vars:      map[string]*slip.VarVal{},
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*class*", &Pkg)
}
