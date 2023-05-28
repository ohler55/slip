// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := FileStringLength{Function: slip.Function{Name: "file-string-length", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "file-string-length",
			Args: []*slip.DocArg{
				{
					Name: "stream",
					Type: "file-stream",
					Text: "The file-stream to return the string-length of.",
				},
				{
					Name: "object",
					Type: "object",
					Text: "The object to determine the length that would be written in bytes.",
				},
			},
			Return: "fixnum",
			Text: `__file-string-length__ returns the number of bytes that would be written to
the stream. This function deviates from the common LISP function in that it allows any object
to be provides as the _object_ argument and not just strings or characters. A _nil_ stream is
also allowed for the _stream_ argument.`,
			Examples: []string{
				`(with-open-file (file "sample.txt" :direction :output) (file-string-length file "abc")) => 3`,
			},
		}, &slip.CLPkg)
}

// File-String-Length represents the file-string-length function.
type FileStringLength struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *FileStringLength) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 2, 2)
	switch args[0].(type) {
	case nil, io.Writer:
	default:
		slip.PanicType("stream", args[0], "output-stream")
	}
	var cnt int
	switch ta := args[1].(type) {
	case slip.String:
		cnt = len(ta)
	case slip.Character:
		cnt = len([]byte(string([]rune{rune(ta)})))
	default:
		cnt = len(slip.Append(nil, args[1]))
	}
	return slip.Fixnum(cnt)
}
