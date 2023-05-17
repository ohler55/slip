// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"os"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileLength{Function: slip.Function{Name: "file-length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-length",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "file-stream",
					Text: "The file-stream to return the length of.",
				},
			},
			Return: "fixnum|nil",
			Text: `__file-length__ returns the length of the _stream_. _nil_ is returned
if the length can not be determined.`,
			Examples: []string{
				`(with-open-file (file "sample.txt" :direction :input) (file-length file)) => 37`,
			},
		}, &slip.CLPkg)
}

// File-Length represents the file-length function.
type FileLength struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FileLength) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 0, 1)
	fs, ok := args[0].(*slip.FileStream)
	if !ok {
		slip.PanicType("stream", args[0], "file-stream")
	}
	if fi, err := (*os.File)(fs).Stat(); err == nil {
		result = slip.Fixnum(fi.Size())
	}
	return
}
