// Copyright (c) 2025, Peter Ohler, All rights reserved.

package swank

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the swank package.
	Pkg = slip.Package{
		Name:      "swank",
		Nicknames: []string{},
		Doc: `Home of symbols defined for the swank package. This package implements
a Swank server for SLIME (Superior Lisp Interaction Mode for Emacs) integration,
enabling interactive Lisp development from Emacs.`,
		PreSet: slip.DefaultPreSet,
	}

	// defaultServer holds the singleton server instance
	defaultServer *Server
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*swank*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
			"*swank-port*": {
				Val:    slip.Fixnum(4005),
				Doc:    "Default port for the Swank server.",
				Export: true,
			},
		},
	)
	defSwankServer()
	defSwankStop()
	defCreateServer()
	defStartServer()
	defStopServer()
	defRestartServer()
	defSetupServer()
	Pkg.Initialize(nil) // lock
	slip.AddPackage(&Pkg)
}
