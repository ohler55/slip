// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Gi package.
	Pkg = slip.Package{
		Name:      "gi",
		Nicknames: []string{"go-integration", "golang-integration"},
		Doc:       "Home of symbols defined for the Go Integration functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(nil)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*gi*", &Pkg)
	Pkg.Set("*go-integration*", &Pkg)
}
