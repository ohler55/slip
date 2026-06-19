// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi

import (
	"fmt"
	"os"
	"os/user"
	"path/filepath"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/flavors"
)

func defLoadSystem() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := LoadSystem{Function: slip.Function{Name: "load-system", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "load-system",
			Args: []*slip.DocArg{
				{
					Name: "system",
					Type: "symbol|keyword|string",
					Text: "System designator of the system to load.",
				},
				{Name: "&optional"},
				{
					Name: "pathname",
					Type: "string|list",
					Text: `Directory the system .asd file is located in or a list of paths
to search for the .asd file in.`,
				},
			},
			Return: "system",
			Text: `__load-system__ load the _system_ by fetching the component files
and loading them using the __:fetch__ and __:load__ methods of the system. Unlike ASDF,
searching for the system is limited to the __*package-load-path*__ unless an oprional
_pathname_ is provided. The search looks for _system_ with a '.asd' extension.`,
			Examples: []string{
				`(load-system  :quux "testdata") => #<system 12345>`,
			},
		}, &Pkg)
}

// LoadSystem represents the load-system function.
type LoadSystem struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *LoadSystem) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	cys := slip.MustBeString(args[0], "system")
	path, _ := s.Get(slip.Symbol("*package-load-path*")).(slip.String)
	var dirs []string
	if 0 < len(path) {
		dirs = append(dirs, string(path))
	}
	if 1 < len(args) {
		switch ta := args[1].(type) {
		case slip.String:
			dirs = []string{string(ta)}
		case slip.List:
			dirs = dirs[:0]
			for _, v := range ta {
				dirs = append(dirs, slip.MustBeString(v, "pathname"))
			}
		default:
			slip.TypePanic(s, depth, "pathname", ta, "string", "list")
		}
	}
	currentPkg := slip.CurrentPackage
	defer func() {
		s.Set(slip.Symbol("*load-pathname*"), nil)
		s.Set(slip.Symbol("*load-truename*"), nil)
		slip.CurrentPackage = currentPkg
	}()
	var (
		buf       []byte
		err       error
		obj       slip.Object
		filepath  string
		dirpath   string
		listProvs slip.ProvSet
	)
	for _, dir := range dirs {
		filepath = fmt.Sprintf("%s/%s.asd", dir, cys)
		s.Set(slip.Symbol("*load-pathname*"), slip.String(filepath))
		s.Set(slip.Symbol("*load-truename*"), slip.String(filepath))
		if buf, err = os.ReadFile(filepath); err != nil {
			continue
		}
		var code slip.Code
		code, listProvs = slip.ReadProv(buf, s, absPath(filepath), listProvs)
		code.CompileWithProvenance(listProvs)
		obj = code.Eval(s, nil)
		dirpath = dir
		break
	}
	if err != nil {
		slip.FilePanic(s, depth, slip.String(filepath), "loading system %s at %s: %s", cys, filepath, err)
	}
	sys, ok := obj.(*flavors.Instance)
	if !ok || sys.Class() != system {
		slip.ErrorPanic(s, depth, "The last expression in %s was not a defsystem.", filepath)
	}
	sys.Set(slip.Symbol("pathname"), slip.String(dirpath))

	_ = sys.Receive(s, ":fetch", nil, depth+1)
	_ = sys.Receive(s, ":load", nil, depth+1)

	return sys
}

func absPath(path string) string {
	if 0 < len(path) && path[0] == '~' {
		if usr, err := user.Current(); err == nil {
			if 1 < len(path) && path[1] == '/' {
				path = filepath.Join(usr.HomeDir, path[2:])
			} else {
				path = filepath.Join(filepath.Dir(usr.HomeDir), path[1:])
			}
		}
	}
	var err error
	if path, err = filepath.Abs(path); err != nil {
		panic(err)
	}
	return path
}
