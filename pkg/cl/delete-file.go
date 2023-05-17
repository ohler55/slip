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
			f := DeleteFile{Function: slip.Function{Name: "delete-file", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "delete-file",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file path to delete.",
				},
			},
			Return: "boolean",
			Text:   `__delete-file__ deletes _filepath_ and returns _t_ if successful and _nil_ otherwise.`,
			Examples: []string{
				`(delete-file "../*.bak") => t`,
			},
		}, &slip.CLPkg)
}

// Delete-File represents the delete-file function.
type DeleteFile struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DeleteFile) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	matches, _ := filepath.Glob(string(path))
	for _, m := range matches {
		if err := os.Remove(m); err == nil {
			result = slip.True
		}
	}
	return
}
