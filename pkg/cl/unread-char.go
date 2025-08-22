// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := UnreadChar{Function: slip.Function{Name: "unread-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "unread-char",
			Args: []*slip.DocArg{
				{
					Name: "character",
					Type: "character",
					Text: "The character to put back on the stream.",
				},
				{Name: "&optional"},
				{
					Name: "stream",
					Type: "input-stream",
					Text: "The stream to unread from. The default is _*standard-input*_",
				},
			},
			Return: "nil",
			Text:   `__unread-char__ places a character as the next character to be read from _stream_.`,
			Examples: []string{
				`(setq is (make-string-input-stream "abc"))`,
				`(read-char is) => #\a`,
				`(unread-char #\x is) => nil`,
				`(read-char is) => #\x`,
			},
		}, &slip.CLPkg)
}

// UnreadChar represents the unread-char function.
type UnreadChar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *UnreadChar) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "character", args[0], "character")
	}
	is := s.Get(slip.Symbol("*standard-input*"))
	if 1 < len(args) {
		is = args[1]
	}
	var sis *slip.InputStream
	if sis, ok = is.(*slip.InputStream); !ok {
		slip.TypePanic(s, depth, "stream", args[1], "input-stream")
	}
	sis.PushRune(rune(c))

	return nil
}
