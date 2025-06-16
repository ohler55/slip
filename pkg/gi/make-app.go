// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"fmt"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeApp{Function: slip.Function{Name: "make-app", Args: args, SkipEval: []bool{true}}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "make-app",
			Args: []*slip.DocArg{
				{
					Name: "name",
					Type: "string",
					Text: "The name of the application being built.",
				},
				{
					Name: "directory",
					Type: "string",
					Text: "The directory to build the application in.",
				},
				{
					Name: "files",
					Type: "list",
					Text: "A list of the file paths to the LISP source code.",
				},
				{
					Name: "entry",
					Type: "string",
					Text: "The name of the function to call on application start.",
				},
				{Name: "&key"},
				{
					Name: "key",
					Type: "string",
					Text: "An encryption key.",
				},
				{
					Name: "key-file",
					Type: "string",
					Text: "The path to a key file (e.g., ~/.app-key).",
				},
				{
					Name: "key-flag",
					Type: "string",
					Text: "A command line flag to accepts an encryption key.",
				},
				{
					Name: "cleanup",
					Type: "boolean",
					Text: "If true then the files used to create the application are removed when finished.",
				},
				{
					Name: "replace",
					Type: "string",
					Text: `If using a development version of SLIP then this key value will be placed in
the go.mod file as a replacement for the slip package.`,
				},
				{
					Name: "plugins",
					Type: "list",
					Text: "A list of the file paths to the plugin packages files (.so files).",
				},
				{
					Name: "usage",
					Type: "string",
					Text: "A string to be used as the usage when the application help is triggered.",
				},
				{
					Name: "options",
					Type: "list",
					Text: `A list property lists. Each property list corresponds to a command line flag
option. The properties are:
  _:flag_ [string] is then flag option. (e.g., for -num use "num")
  _:doc_ [string] defines the help documentation string.
  _:default_ [object] specifies a type appropriate default value.
  _:type_ [symbol] a symbol to be used to coerce the flag value into the desired type.
  _:var_ [symbol] is the symbol to bind the flag value to.
`,
				},
			},
			Return: "nil",
			Text:   `__make-app__ builds a standalone application with optional encrypted source files.`,
			Examples: []string{
				`(make-app "quux" "scratch" '("quux.lisp") 'quux)`,
			},
		}, &Pkg)
}

// MakeApp represents the make-app function.
type MakeApp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeApp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 4, 20)
	app := slip.App{
		Title: slip.MustBeString(args[0], "name"),
	}
	dir := slip.MustBeString(args[1], "directory")
	if list, ok := args[2].(slip.List); ok {
		app.LispCode = make([]string, len(list))
		for i, v := range list {
			app.LispCode[i] = slip.MustBeString(v, "files")
		}
	} else {
		slip.PanicType("directory", args[2], "list")
	}
	var (
		key     []byte
		replace string
		cleanup bool
	)
	rest := args[4:]
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("key")); has {
		key = []byte(slip.MustBeString(v, "key"))
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("key-flag")); has {
		app.KeyFlag = slip.MustBeString(v, "key-flag")
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("key-file")); has {
		app.KeyFile = slip.MustBeString(v, "key-file")
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("cleanup")); has {
		cleanup = v != nil
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("replace")); has {
		replace = slip.MustBeString(v, "replace")
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("options")); has {
		if list, ok := v.(slip.List); ok {
			app.Plugins = make([]string, len(list))
			for i, v := range list {
				app.Plugins[i] = slip.MustBeString(v, "plugins")
			}
		} else {
			slip.PanicType("plugins", v, "list")
		}
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("usage")); has {
		usage := slip.MustBeString(v, "usage")
		app.Usage = func() { fmt.Println(usage) }
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol("options")); has {
		if list, ok := v.(slip.List); ok {
			app.Options = make([]*slip.AppArg, len(list))
			for i, v2 := range list {
				app.Options[i] = f.appArgFromPlist(v2)
			}
		} else {
			slip.PanicType("options", v, "property-list")
		}
	}
	app.Generate(dir, key, replace, cleanup)

	return nil
}

func (f *MakeApp) appArgFromPlist(v slip.Object) *slip.AppArg {
	plist, ok := v.(slip.List)
	if !ok {
		slip.PanicType("options", v, "property-list")
	}
	plen := len(plist)
	var aa slip.AppArg
	for i := 0; i < plen-1; i += 2 {
		switch plist[i] {
		case slip.Symbol(":flag"):
			aa.Flag = slip.MustBeString(plist[i+1], ":flag")
		case slip.Symbol(":doc"):
			aa.Doc = slip.MustBeString(plist[i+1], ":doc")
		case slip.Symbol(":default"):
			aa.Default = plist[i+1]
		case slip.Symbol(":type"):
			aa.Type = slip.MustBeString(plist[i+1], ":type")
		case slip.Symbol(":var"):
			aa.Var = slip.MustBeString(plist[i+1], ":var")
		default:
			slip.PanicType("preperty", plist[i], ":flag", ":doc", ":default", ":type", ":var")
		}
	}
	return &aa
}
