// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ReadChar{Function: slip.Function{Name: "read-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "read-char",
			Args: []*slip.DocArg{
				{Name: "&optional"},
				{
					Name: "stream",
					Type: "input-stream",
					Text: "The stream to read from. The default is _*standard-input*_",
				},
				{
					Name: "eof-error-p",
					Type: "boolean",
					Text: `If true an EOF error is raised when a read attempt is made at
the end of the stream. The default is _t_.`,
				},
				{
					Name: "eof-value",
					Type: "object",
					Text: "The value to return on EOF if _eof-error-p_ is nil.",
				},
				{
					Name: "recursive-p",
					Type: "boolean",
					Text: "If true the call is embedded in a higher level function. Has no impact on behavior.",
				},
			},
			Return: "input-stream",
			Text:   `__read-char__ reads a character from the _stream_.`,
			Examples: []string{
				`(read-char (make-string-input-stream "abc")) => #\a>`,
			},
		}, &slip.CLPkg)
}

// ReadChar represents the read-char function.
type ReadChar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ReadChar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 0, 4)
	var is slip.Object = s.Get(slip.Symbol("*standard-input*"))
	if 0 < len(args) {
		is = args[0]
	}
	var rr io.RuneReader
	switch tis := is.(type) {
	case *slip.InputStream:
		if rr, _ = tis.Reader.(io.RuneReader); rr == nil {
			// TBD wrap tis with rune reader
		}
	default:
		// TBD also check io.Reader
		// maybe better to implement RuneReader on input stream as well as others like unread
	}
	// TBD make sure

	return nil
}
