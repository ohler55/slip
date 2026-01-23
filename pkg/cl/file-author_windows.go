//go:build windows

// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileAuthor{Function: slip.Function{Name: "file-author", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-author",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file to get the author of.",
				},
			},
			Return: "string|nil",
			Text:   `__file-author__ returns author of the _filepath_ or _nil_ if the author can not be determined.`,
			Examples: []string{
				`(file-author "three.lisp") => "arthur"`,
			},
		}, &slip.CLPkg)
}

// FileAuthor represents the file-author function.
type FileAuthor struct {
	slip.Function
}

// Call the function with the arguments provided.
// On Windows, file ownership works differently, so we return nil.
func (f *FileAuthor) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "filepath", args[0], "string")
	}
	// On Windows, getting file author is not straightforward like on Unix.
	// We check if the file exists and return nil for the author.
	if _, err := os.Stat(string(path)); err == nil {
		// File exists but we can't easily get the owner on Windows
		// without using more complex Windows APIs
		return nil
	}
	return nil
}
