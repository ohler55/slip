// Copyright (c) 2023, Peter Ohler, All rights reserved.

package net

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the net package.
	Pkg = slip.Package{
		Name:      "net",
		Nicknames: []string{"networking", "network"},
		Doc:       "Home of symbols defined for the net (networking) package.",
		PreSet:    slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*net*": {Val: &Pkg, Doc: Pkg.Doc, Export: true},
		},
	)
	defClient()
	defRequest()
	defResponse()
	defResponseWriter()
	defServer()
	defUsocket()

	Pkg.Initialize(nil, &bodyWrap{}) // lock
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
