// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileNamestring{Function: slip.Function{Name: "file-namestring", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-namestring",
			Args: []*slip.DocArg{
				{
					Name: "pathname",
					Type: "string",
					Text: "The file to get the base name from.",
				},
			},
			Return: "string",
			Text:   `__file-namestring__ returns the base name of the _file_.`,
			Examples: []string{
				`(file-namestring "../one/two/three.lisp") => "three.lisp"`,
			},
		}, &slip.CLPkg)
}

// FileNamestring represents the file-namestring function.
type FileNamestring struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FileNamestring) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("pathname", args[0], "string")
	}
	return slip.String(filepath.Base(string(path)))
}
