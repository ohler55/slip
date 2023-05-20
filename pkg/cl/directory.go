// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Directory{Function: slip.Function{Name: "directory", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "directory",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "The directory path to get the pathnames for.",
				},
			},
			Return: "list",
			Text: `__directory__ returns a list of pathnames matching the _path_
specification which can include wildcards.`,
			Examples: []string{
				`(directory "../*.lisp") => ("/top/one/x.lisp" "/top/one/y.lisp")`,
			},
		}, &slip.CLPkg)
}

// Directory represents the directory function.
type Directory struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Directory) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("path", args[0], "string")
	}
	spath, _ := filepath.Abs(string(path))
	matches, _ := filepath.Glob(spath)
	var mlist slip.List
	for _, m := range matches {
		mlist = append(mlist, slip.String(m))
	}
	return mlist
}
