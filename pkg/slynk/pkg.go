// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slynk

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the slynk package.
	Pkg = slip.Package{
		Name:      "slynk",
		Nicknames: []string{},
		Doc: `Home of symbols defined for the slynk package. This package implements
a Slynk server for SLY (Sylvester the Cat's Common Lisp IDE) integration,
enabling interactive Lisp development from Emacs with enhanced features like
multiple REPLs, channels, and flex-completion.`,
		PreSet: slip.DefaultPreSet,
	}

	// defaultServer holds the singleton server instance
	defaultServer *Server
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*slynk*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
			"*slynk-port*": {
				Val:    slip.Fixnum(4005),
				Doc:    "Default port for the Slynk server.",
				Export: true,
			},
		},
	)
	defSlynkServer()
	defSlynkStop()
	Pkg.Initialize(nil) // lock
	slip.AddPackage(&Pkg)
}
