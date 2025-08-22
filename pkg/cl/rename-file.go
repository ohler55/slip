// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"os"
	"path/filepath"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := RenameFile{Function: slip.Function{Name: "rename-file", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "rename-file",
			Args: []*slip.DocArg{
				{
					Name: "filename",
					Type: "string",
					Text: "The file to rename.",
				},
				{
					Name: "new-name",
					Type: "string",
					Text: "The new file name.",
				},
			},
			Return: "string,string,string",
			Text: `__rename-file__ returns the new name, the full old name, and the
full new name if successful. On error a panic is raised.`,
			Examples: []string{
				`(rename-file "foo.lisp" "bar.lisp) => "bar.lisp", "/top/foo.lisp", "/top/bar.lisp"`,
			},
		}, &slip.CLPkg)
}

// RenameFile represents the rename-file function.
type RenameFile struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *RenameFile) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	name, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "filename", args[0], "string")
	}
	var newName slip.String
	if newName, ok = args[1].(slip.String); !ok {
		slip.TypePanic(s, depth, "new-name", args[1], "string")
	}
	oldPath, _ := filepath.Abs(string(name))
	newPath, _ := filepath.Abs(string(newName))
	if err := os.Rename(oldPath, newPath); err != nil {
		slip.NewPanic("rename %s to %s failed. %s", oldPath, newPath, err)
	}
	return slip.Values{newName, slip.String(oldPath), slip.String(newPath)}
}
