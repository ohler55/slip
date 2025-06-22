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
	Pkg.Initialize(map[string]*slip.VarVal{
		"*xml*": {Val: &Pkg, Const: true, Export: true, Doc: "The XML package."},
	})
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Initialize(nil, &Read{}) // lock
}
