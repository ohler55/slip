// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeApp{Function: slip.Function{Name: "make-app", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Kind: slip.MacroSymbol,
			Name: "make-app",
			Args: []*slip.DocArg{
				{
					Name: "app-filepath",
					Type: "string",
					Text: "The filepath including the application name of the application being built.",
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
					Name: "scratch",
					Type: "string",
					Text: "The directory to build the application in. (default: a temporary directory)",
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
					Text: "A list of the plugin names or full filepaths to the files (.so files).",
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
			Return: "string|nil",
			Text: `__make-app__ builds a standalone application with optional encrypted source files. The scratch
directory path to the files used to build the application is returned unless _:cleanup_ was true in which case
__nil__ is returned.`,
			Examples: []string{
				`(make-app "quux" '("quux.lisp") 'quux) => /tmp/scratch`,
			},
		}, &Pkg)
}

// MakeApp represents the make-app function.
type MakeApp struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeApp) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 3, 20)
	appPath := slip.MustBeString(args[0], "app-filepath")
	app := slip.App{
		Title:         filepath.Base(appPath),
		EntryFunction: slip.MustBeString(args[2], "entry"),
	}
	if list, ok := args[1].(slip.List); ok {
		app.LispCode = make([]string, len(list))
		for i, v := range list {
			app.LispCode[i] = slip.MustBeString(v, "files")
		}
	} else {
		slip.PanicType("app-filepath", args[2], "list")
	}
	var (
		key        []byte
		replace    string
		cleanup    bool
		scratchDir string
	)
	rest := args[3:]
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":key")); has {
		key = []byte(slip.MustBeString(v, ":key"))
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":key-flag")); has {
		app.KeyFlag = slip.MustBeString(v, ":key-flag")
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":key-file")); has {
		app.KeyFile = slip.MustBeString(v, ":key-file")
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":scratch")); has {
		scratchDir = slip.MustBeString(v, ":scratch")
	}
	if len(scratchDir) == 0 {
		scratchDir = filepath.Join(os.TempDir(), "scratch")
		_ = os.RemoveAll(scratchDir) // make sure then directory is clean
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":cleanup")); has {
		cleanup = v != nil
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":replace")); has {
		replace = slip.MustBeString(v, ":replace")
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":plugins")); has {
		if list, ok := v.(slip.List); ok {
			app.Plugins = make([]string, len(list))
			for i, v := range list {
				app.Plugins[i] = f.findPluginPath(s, slip.MustBeString(v, ":plugins"))
			}
		} else {
			slip.PanicType(":plugins", v, "list")
		}
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":usage")); has {
		usage := slip.MustBeString(v, ":usage")
		app.Usage = func() { fmt.Println(usage) }
	}
	if v, has := slip.GetArgsKeyValue(rest, slip.Symbol(":options")); has {
		if list, ok := v.(slip.List); ok {
			app.Options = make([]*slip.AppArg, len(list))
			for i, v2 := range list {
				app.Options[i] = f.appArgFromPlist(v2)
			}
		} else {
			slip.PanicType(":options", v, "property-list")
		}
	}
	app.Generate(scratchDir, key, replace, cleanup)
	srcApp := filepath.Join(scratchDir, app.Title)
	if err := os.Rename(srcApp, appPath); err != nil {
		slip.NewPanic("renaming %s to %s failed. %s", srcApp, appPath, err)
	}
	if cleanup {
		_ = os.RemoveAll(scratchDir)
		return nil
	}
	return slip.String(scratchDir)
}

func (f *MakeApp) findPluginPath(s *slip.Scope, name string) (path string) {
	name = expandPath(name)
	if strings.HasSuffix(name, ".so") {
		if _, err := os.Stat(name); err == nil {
			return name
		}
	}
	var paths []string
	lp := s.Get("*package-load-path*")
	switch tp := lp.(type) {
	case nil:
	case slip.String:
		paths = append(paths, expandPath(string(tp)))
	case slip.List:
		for _, a2 := range tp {
			if str, ok := a2.(slip.String); ok {
				paths = append(paths, expandPath(string(str)))
			} else {
				slip.PanicType("*package-load-path* members", a2, "string")
			}
		}
	default:
		slip.PanicType("*package-load-path*", lp, "string", "list of strings")
	}
	for _, p := range paths {
		filepath := fmt.Sprintf("%s/%s.so", p, name)
		if _, err := os.Stat(filepath); err == nil {
			path = filepath
			break
		}
	}
	if len(path) == 0 {
		slip.NewPanic("could not find plugin %s.", name)
	}
	return
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

func expandPath(path string) string {
	if 0 < len(path) && path[0] == '~' {
		home := os.Getenv("HOME")
		path = home + path[1:]
	}
	return path
}
