// Copyright (c) 2025, Peter Ohler, All rights reserved.

package alive

import (
	"github.com/ohler55/slip"
)

var (
	// Pkg is the alive package.
	Pkg = slip.Package{
		Name:      "alive",
		Nicknames: []string{},
		Doc: `Home of symbols defined for the alive package. This package implements
an LSP server for VSCode's Alive extension, enabling interactive Lisp
development from VSCode with features like evaluation, completion, and
hover documentation.`,
		PreSet: slip.DefaultPreSet,
	}

	// defaultServer holds the singleton server instance
	defaultServer *Server
)

func init() {
	Pkg.Initialize(
		map[string]*slip.VarVal{
			"*alive*": {
				Val:    &Pkg,
				Doc:    Pkg.Doc,
				Const:  true,
				Export: true,
			},
			"*alive-port*": {
				Val:    slip.Fixnum(4006),
				Doc:    "Default port for the Alive LSP server.",
				Export: true,
			},
		},
	)
	defAliveServer()
	defAliveStop()
	Pkg.Initialize(nil) // lock
	slip.AddPackage(&Pkg)
}
