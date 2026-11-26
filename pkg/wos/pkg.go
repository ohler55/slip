// Copyright (c) 2022, Peter Ohler, All rights reserved.

package wos

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the Wos package.
	Pkg = slip.Package{
		Name:      "wos",
		Nicknames: []string{},
		Doc: `Home of symbols defined for the wos package. A wos is a
Well Ordered Set (WOS) or a Whole number Ordered Set if preferred. It
is a set of values indexed by non-negative fixnums.`,
		PreSet: slip.DefaultPreSet,
	}
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{
		"*wos*": {
			Val:    &Pkg,
			Doc:    Pkg.Doc,
			Const:  true,
			Export: true,
		},
	})
	f := defWos()
	vv := Pkg.GetVarVal(f.Name())
	vv.Const = true

	// Pkg.Initialize(nil, &Get{}) // lock it down
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}
