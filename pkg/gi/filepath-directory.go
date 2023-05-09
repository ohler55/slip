// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FilepathDirectory{Function: slip.Function{Name: "filepath-directory", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "filepath-directory",
			Args: []*slip.DocArg{
				{
					Name: "path",
					Type: "string",
					Text: "The file path to get the directory from.",
				},
			},
			Return: "string",
			Text:   `__filepath-directory__ returns the directory of the _path_.`,
			Examples: []string{
				`(filepath-directory "../one/two/three.lisp") => "../one/two")`,
			},
		}, &slip.CLPkg)
}

// FilepathDirectory represents the filepath-directory function.
type FilepathDirectory struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FilepathDirectory) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	return slip.String(filepath.Dir(string(path)))
}
