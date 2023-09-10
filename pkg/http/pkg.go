// Copyright (c) 2023, Peter Ohler, All rights reserved.

package http

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the HTTP package.
	Pkg = slip.Package{
		Name:      "http",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the http package.",
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
		PreSet:    slip.DefaultPreSet,
		Vars:      map[string]*slip.VarVal{},
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*http*", &Pkg)
}
