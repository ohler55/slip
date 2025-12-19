// Copyright (c) 2022, Peter Ohler, All rights reserved.

package repl

import (
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
)

const (
	printANSI       = "*print-ansi*"
	stdInput        = "*standard-input*"
	stdOutput       = "*standard-output*"
	editorFlagsName = "*repl-editor-flags*"

	form1Key  = "+"
	form2Key  = "++"
	form3Key  = "+++"
	value1Key = "/"
	value2Key = "//"
	value3Key = "///"

	configHeader = ";;;; slip REPL configuration file. For help type: (help 'configuration)\n\n"
)

var (
	Pkg = slip.Package{
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
		"*repl*":        {Val: &Pkg, Const: true, Export: true, Doc: "The REPL package."},
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
			Const:  true,
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
		"*bold*": {
			Val:   slip.String("\x1b[1m"),
			Const: true,
			Doc:   "bold ANSI sequence",
		},
		"*underline*": {
			Val:   slip.String("\x1b[4m"),
			Const: true,
			Doc:   "underline ANSI sequence",
		},
		"*ansi-reset*": {
			Val:   slip.String("\x1b[m"),
			Const: true,
			Doc:   "reset ANSI sequence",
		},
		"*ansi-black*": {
			Val:   slip.String("\x1b[30m"),
			Const: true,
			Doc:   "black ANSI sequence",
		},
		"*ansi-red*": {
			Val:   slip.String("\x1b[31m"),
			Const: true,
			Doc:   "red ANSI sequence",
		},
		"*ansi-green*": {
			Val:   slip.String("\x1b[32m"),
			Const: true,
			Doc:   "green ANSI sequence",
		},
		"*ansi-yellow*": {
			Val:   slip.String("\x1b[33m"),
			Const: true,
			Doc:   "yellow ANSI sequence",
		},
		"*ansi-blue*": {
			Val:   slip.String("\x1b[34m"),
			Const: true,
			Doc:   "blue ANSI sequence",
		},
		"*ansi-magenta*": {
			Val:   slip.String("\x1b[35m"),
			Const: true,
			Doc:   "magenta ANSI sequence",
		},
		"*ansi-cyan*": {
			Val:   slip.String("\x1b[36m"),
			Const: true,
			Doc:   "cyan ANSI sequence",
		},
		"*ansi-white*": {
			Val:   slip.String("\x1b[37m"),
			Const: true,
			Doc:   "white ANSI sequence",
		},
		"*ansi-light-black*": {
			Val:   slip.String("\x1b[90m"),
			Const: true,
			Doc:   "gray ANSI sequence",
		},
		"*ansi-light-red*": {
			Val:   slip.String("\x1b[91m"),
			Const: true,
			Doc:   "light-red ANSI sequence",
		},
		"*ansi-light-green*": {
			Val:   slip.String("\x1b[92m"),
			Const: true,
			Doc:   "light-green ANSI sequence",
		},
		"*ansi-light-yellow*": {
			Val:   slip.String("\x1b[93m"),
			Const: true,
			Doc:   "light-yellow ANSI sequence",
		},
		"*ansi-light-blue*": {
			Val:   slip.String("\x1b[94m"),
			Const: true,
			Doc:   "light-blue ANSI sequence",
		},
		"*ansi-light-magenta*": {
			Val:   slip.String("\x1b[95m"),
			Const: true,
			Doc:   "light-magenta ANSI sequence",
		},
		"*ansi-light-cyan*": {
			Val:   slip.String("\x1b[96m"),
			Const: true,
			Doc:   "light-cyan ANSI sequence",
		},
		"*ansi-light-white*": {
			Val:   slip.String("\x1b[97m"),
			Const: true,
			Doc:   "bright-white ANSI sequence",
		},
	}, &Ansi{})

	defReplFunction()

	slip.AddPackage(&Pkg)
	slip.UserPkg.Use(&Pkg)
}

func getPrompt() slip.Object {
	return slip.String(prompt)
}

func setPrompt(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		prompt = string(str)
		if ed, ok := replReader.(*editor); ok {
			ed.foff = printSize(prompt) + 1 // terminal positions are one based and not zero based so add one
		}
	} else {
		panic("*repl-prompt* must be a string")
	}
}

func getMatchColor() slip.Object {
	return slip.String(matchColor)
}

func setMatchColor(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		matchColor = string(str)
	} else {
		panic("*repl-match-color* must be a string")
	}
}

func getEditorFlags() slip.Object {
	return editorFlags
}

func setEditorFlags(value slip.Object) {
	switch list := value.(type) {
	case nil:
		editorFlags = slip.List{}
	case slip.List:
		editorFlags = list
	default:
		panic("*repl-editor-flags* must be a list of strings")
	}
}

func getExternalEditor() slip.Object {
	return slip.String(externalEditor)
}

func setExternalEditor(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		externalEditor = string(str)
	} else {
		panic("*repl-external-editor* must be a string")
	}
}

func getWarnPrefix() slip.Object {
	return slip.String(warnPrefix)
}

func setWarnPrefix(value slip.Object) {
	if str, ok := value.(slip.String); ok {
		warnPrefix = string(str)
	} else {
		panic("*repl-warning-prefix* must be a string")
	}
}

func getEditor() slip.Object {
	if _, ok := replReader.(*editor); ok {
		return slip.True
	}
	return nil
}

func setEditor(value slip.Object) {
	if value == nil {
		if _, ok := replReader.(*termReader); !ok {
			replReader = &termReader{}
			replReader.initialize()
		}
	} else if _, ok := replReader.(*editor); !ok {
		replReader = &editor{}
		replReader.initialize()
	}
}

func getEvalOnClose() slip.Object {
	if evalOnClose {
		return slip.True
	}
	return nil
}

func setEvalOnClose(value slip.Object) {
	evalOnClose = value != nil
}

func SetSizer(hs hasSize) {
	sizer = hs
}

func getInteractive() slip.Object {
	if Interactive {
		return slip.True
	}
	return nil
}

func setInteractive(_ slip.Object) {
	panic("*repl-interactive* is a read only variable")
}

func getStashLoadPath() slip.Object {
	return stashLoadPath
}

func setStashLoadPath(value slip.Object) {
	switch list := value.(type) {
	case nil:
		stashLoadPath = slip.List{}
	case slip.List:
		stashLoadPath = list
	default:
		panic("*stash-load-path* must be a list of strings")
	}
}

func getDefaultStashName() (name slip.Object) {
	if 0 < len(defaultStashName) {
		name = slip.String(defaultStashName)
	}
	return
}

func setDefaultStashName(value slip.Object) {
	switch tv := value.(type) {
	case nil:
		defaultStashName = ""
	case slip.String:
		defaultStashName = string(tv)
	case slip.Symbol:
		defaultStashName = string(tv)
	default:
		panic("*default-stash-name* must be a string or nil")
	}
	if 0 < len(defaultStashName) {
		dir := FindConfigDir()
		if err := os.MkdirAll(dir, 0755); err != nil {
			panic(err)
		}
		forms := TheStash.forms
		TheStash.LoadExpanded(filepath.Join(dir, defaultStashName))
		TheStash.forms = append(TheStash.forms, forms...)
	}
}
