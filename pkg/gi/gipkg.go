// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

var (
	// GiPkg is the Gi package.
	GiPkg = slip.Package{
		Name:      "gi",
		Nicknames: []string{"go-integration", "golang-integration"},
		Doc:       "Home of symbols defined for the Go Integration functions, variables, and constants.",
		Vars:      map[string]*slip.VarVal{},
		Lambdas:   map[string]*slip.Lambda{},
		Funcs:     map[string]*slip.FuncInfo{},
	}
)

func init() {
	slip.AddPackage(&GiPkg)
	slip.UserPkg.Use(&GiPkg)
	GiPkg.Set("*gi*", &GiPkg)
	GiPkg.Set("*go-integration*", &GiPkg)
}
