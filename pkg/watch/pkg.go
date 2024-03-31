// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the watch package.
	Pkg = slip.Package{
		Name:      "watch",
		Nicknames: []string{},
		Doc:       "Home of symbols defined for the watch package.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{})
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*watch*", &Pkg)
}
