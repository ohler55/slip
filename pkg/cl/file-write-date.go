// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileWriteDate{Function: slip.Function{Name: "file-write-date", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-write-date",
			Args: []*slip.DocArg{
				{
					Name: "filepath",
					Type: "string",
					Text: "The file to get the write date of.",
				},
			},
			Return: "time|nil",
			Text:   `__file-write-date__ returns write time of the _filepath_.`,
			Examples: []string{
				`(file-write-date "three.lisp") => "xx"`,
			},
		}, &slip.CLPkg)
}

// FileWriteDate represents the file-write-date function.
type FileWriteDate struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FileWriteDate) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	path, ok := args[0].(slip.String)
	if !ok {
		slip.TypePanic(s, depth, "filepath", args[0], "string")
	}
	fi, err := os.Stat(string(path))
	if err != nil {
		slip.ErrorPanic(s, depth, "file stat on %s failed. %s", path, err)
	}
	return slip.Time(fi.ModTime())
}
