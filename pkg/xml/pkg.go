// Copyright (c) 2023, Peter Ohler, All rights reserved.

package xml

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the xml package.
	Pkg = slip.Package{
		Name:      "xml",
		Nicknames: []string{"xml"},
		Doc:       "Home of symbols defined for the XML functions, variables, and constants.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(nil)
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*xml*", &Pkg)
}
