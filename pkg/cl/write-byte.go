// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl

import (
	"io"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := WriteByte{Function: slip.Function{Name: "write-byte", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "write-byte",
			Args: []*slip.DocArg{
				{
					Name: "byte",
					Type: "integer",
					Text: "The byte, and integer to write.",
				},
				{
					Name: "stream",
					Type: "output-stream",
					Text: "The stream to write to.",
				},
			},
			Return: "integer",
			Text: `__write-byte__ writes a byte as a character to the provided _stream_. The
_byte_ is returned.`,
			Examples: []string{
				"(write-byte 65 *standard-output*) => 65 ;; A is written",
			},
		}, &slip.CLPkg)
}

// WriteByte represents the write-byte function.
type WriteByte struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *WriteByte) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	w, ok := args[1].(io.Writer)
	if !ok {
		slip.TypePanic(s, depth, "stream", args[1], "output-stream")
	}
	var b byte
	switch ta := args[0].(type) {
	case slip.Fixnum:
		if ta < 0 || 255 < ta {
			slip.TypePanic(s, depth, "byte", args[0], "fixnum")
		}
		b = byte(ta)
	case slip.Octet:
		b = byte(ta)
	default:
		slip.TypePanic(s, depth, "byte", args[0], "fixnum")
	}
	if _, err := w.Write([]byte{b}); err != nil {
		slip.StreamPanic(s, depth, args[1].(slip.Stream), "write-byte failed. %s", err)
	}
	return slip.Octet(b)
}
