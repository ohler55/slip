// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

var (
	Pkg = slip.Package{
		Name:      "repl",
		Nicknames: []string{},
		Doc:       "A package for REPL related functions.",
		Vars: map[string]*slip.VarVal{
			"*repl-prompt*":         {Get: getPrompt, Set: setPrompt, Doc: "the REPL prompt"},
			"*repl-warning-prefix*": {Get: getWarnPrefix, Set: setWarnPrefix, Doc: "prefix to print before a warning"},
			"*repl-editor*": {
				Get: getEditor,
				Set: setEditor,
				Doc: "if true use the SLIP REPL editor as the reader else use a simple line reader.",
			},
		},
		Lambdas: map[string]*slip.Lambda{},
		Funcs:   map[string]*slip.FuncInfo{},
		Locked:  true,
	}
)

func init() {
	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	Pkg.Set("*repl*", &Pkg)
	for _, vv := range Pkg.Vars {
		vv.Pkg = &Pkg
	}
}
