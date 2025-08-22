// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WriteChar{Function: slip.Function{Name: "write-char", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write-char",
			Args: []*slip.DocArg{
				{
					Name: "char",
					Type: "character",
					Text: "The character to write.",
				},
				{Name: "&optional"},
				{
					Name: "output-stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "character",
			Text:   `__write-char__ writes _char_ to _output-stream_. The _char_ is returned.`,
			Examples: []string{
				`(write-char #\A *standard-output*) => #\A ;; A is written`,
			},
		}, &slip.CLPkg)
}

// WriteChar represents the write-char function.
type WriteChar struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteChar) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.CheckArgCount(s, depth, f, args, 1, 2)
	so := s.Get("*standard-output*")
	w := so.(io.Writer)
	ss, _ := so.(slip.Stream)
	c, ok := args[0].(slip.Character)
	if !ok {
		slip.TypePanic(s, depth, "char", args[0], "character")
	}
	if 1 < len(args) {
		if w, ok = args[1].(io.Writer); !ok {
			slip.TypePanic(s, depth, "output-stream", args[1], "output-stream")
		}
		ss, _ = args[1].(slip.Stream)
	}

	if _, err := w.Write([]byte(string([]rune{rune(c)}))); err != nil {
		slip.PanicStream(ss, "write-char failed. %s", err)
	}
	return c
}
