// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DirectoryNamestring{Function: slip.Function{Name: "directory-namestring", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "directory-namestring",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file path to get the directory from.",
				},
			},
			Return: "string",
			Text:   `__directory-namestring__ returns the directory of the _filepath_.`,
			Examples: []string{
				`(directory-namestring "../one/two/three.lisp") => "../one/two")`,
			},
		}, &slip.CLPkg)
}

// DirectoryNamestring represents the directory-namestring function.
type DirectoryNamestring struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DirectoryNamestring) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("filepath", args[0], "string")
	}
	return slip.String(filepath.Dir(string(path)))
}
