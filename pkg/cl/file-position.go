// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FilePosition{Function: slip.Function{Name: "file-position", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-position",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "file-stream",
					Text: "The file-stream to return the position of.",
				},
				{Name: "&optional"},
				{
					Name: "position",
					Type: "fixnum|nil",
					Text: "The position to move to or nil to return the current position in the stream.",
				},
			},
			Return: "fixnum|nil",
			Text: `__file-position__ returns the position in the _stream_ if _position_ is nil or
sets the position in the _stream_ if a valid _position_ is provided.`,
			Examples: []string{
				`(with-open-file (file "sample.txt" :direction :input) (file-position file)) => 0`,
			},
		}, &slip.CLPkg)
}

// File-Position represents the file-position function.
type FilePosition struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FilePosition) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	seeker, ok := args[0].(io.Seeker)
	if !ok {
		slip.PanicType("stream", args[0], "file-stream", "broadcast-stream", "synonym-stream")
	}
	var offset int64
	whence := 1
	if 1 < len(args) {
		if pos, ok := args[1].(slip.Fixnum); ok {
			offset = int64(pos)
			whence = 0
		} else {
			slip.PanicType("position", args[1], "fixnum")
		}
	}
	if pos, err := seeker.Seek(offset, whence); err == nil {
		result = slip.Fixnum(pos)
	}
	return
}
