// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"github.com/ohler55/slip"
)

var (
	pkgVarVal = slip.VarVal{Set: setREPL, Doc: "the REPL package", Export: true}
	Pkg       = slip.Package{
		Name:      "repl",
		Nicknames: []string{},
		Doc:       "A package for REPL related functions.",
		PreSet:    slip.DefaultPreSet,
	}
	// TheHistory is the single global used by readers (editor and
	// termReader).
	TheHistory History
	TheStash   Stash
)

func init() {
	Pkg.Initialize(map[string]*slip.VarVal{
		"*repl*":        &pkgVarVal,
		"*repl-prompt*": {Get: getPrompt, Set: setPrompt, Doc: "the REPL prompt", Export: true},
		"*repl-external-editor*": {
			Get:    getExternalEditor,
			Set:    setExternalEditor,
			Doc:    "The REPL external editor application",
			Export: true,
		},
		"*repl-editor-flags*": {
			Get:    getEditorFlags,
			Set:    setEditorFlags,
			Doc:    "The REPL external editor flags",
			Export: true,
		},
		"*repl-warning-prefix*": {
			Get:    getWarnPrefix,
			Set:    setWarnPrefix,
			Doc:    "Prefix to print before a warning",
			Export: true,
		},
		"*repl-match-color*": {
			Get:    getMatchColor,
			Set:    setMatchColor,
			Doc:    "Sets the ANSI sequence for matching parenthesis colorization",
			Export: true,
		},
		"*repl-editor*": {
			Get:    getEditor,
			Set:    setEditor,
			Doc:    "If true use the SLIP REPL editor as the reader else use a simple line reader.",
			Export: true,
		},
		"*repl-history-limit*": {
			Get:    getHistoryLimit,
			Set:    setHistoryLimit,
			Doc:    "The form history limit.",
			Export: true,
		},
		"*repl-help-box*": {Val: slip.True, Doc: "If true display help in a box.", Export: true},
		"*repl-debug*":    {Val: nil, Doc: "If true the go stack is printed on error.", Export: true},
		"*repl-eval-on-close*": {
			Get:    getEvalOnClose,
			Set:    setEvalOnClose,
			Doc:    "If true evaluate a form when the close parenthensis typed.",
			Export: true,
		},
		"*repl-interactive*": {
			Get:    getInteractive,
			Set:    setInteractive,
			Doc:    "True if the repl is interactive.",
			Export: true,
		},
		"*default-stash-name*": {
			Get:    getDefaultStashName,
			Set:    setDefaultStashName,
			Doc:    `The default stash name used on REPL startup unless overridden. The default value is "stash.lisp"`,
			Export: true,
		},
		"*stash-load-path*": {
			Get: getStashLoadPath,
			Set: setStashLoadPath,
			Doc: `The stash load paths to search when calling the _use-stash_ function if a full path is not provided.
The default value is ("." "~/.config/slip" "~/.slip")`,
			Export: true,
		},
	}, &Ansi{})

	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
	pkgVarVal.Get = getREPL

	slip.DefConstant(&Pkg, "*bold*", slip.String("\x1b[1m"), "bold ANSI sequence")
	slip.DefConstant(&Pkg, "*underline*", slip.String("\x1b[4m"), "underline ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-reset*", slip.String("\x1b[m"), "reset ANSI sequence")

	slip.DefConstant(&Pkg, "*ansi-black*", slip.String("\x1b[30m"), "black ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-red*", slip.String("\x1b[31m"), "red ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-green*", slip.String("\x1b[32m"), "green ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-yellow*", slip.String("\x1b[33m"), "yellow ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-blue*", slip.String("\x1b[34m"), "blue ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-magenta*", slip.String("\x1b[35m"), "magenta ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-cyan*", slip.String("\x1b[36m"), "cyan ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-white*", slip.String("\x1b[37m"), "white ANSI sequence")

	slip.DefConstant(&Pkg, "*ansi-light-black*", slip.String("\x1b[90m"), "gray ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-red*", slip.String("\x1b[91m"), "light-red ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-green*", slip.String("\x1b[92m"), "light-green ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-yellow*", slip.String("\x1b[93m"), "light-yellow ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-blue*", slip.String("\x1b[94m"), "light-blue ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-magenta*", slip.String("\x1b[95m"), "light-magenta ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-cyan*", slip.String("\x1b[96m"), "light-cyan ANSI sequence")
	slip.DefConstant(&Pkg, "*ansi-light-white*", slip.String("\x1b[97m"), "bright-white ANSI sequence")
}
