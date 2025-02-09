// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MakeStringInputStream{Function: slip.Function{Name: "make-string-input-stream", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "make-string-input-stream",
			Args: []*slip.DocArg{
				{
					Name: "string",
					Type: "string",
					Text: "The string to read from.",
				},
				{Name: "&optional"},
				{
					Name: "start",
					Type: "fixnum|nil",
					Text: "The start of the bounds of the _string_.",
				},
				{
					Name: "end",
					Type: "fixnum|nil",
					Text: "The end of the bounds of the _string_. _nil_ indicated the length of _string_.",
				},
			},
			Return: "input-stream",
			Text:   `__make-string-input-stream__ returns an _input-stream_ backed by _string_.`,
			Examples: []string{
				`(make-string-input-stream "this is the input") => #<INPUT-STREAM>`,
			},
		}, &slip.CLPkg)
}

// MakeStringInputStream represents the make-string-input-stream function.
type MakeStringInputStream struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MakeStringInputStream) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.ArgCountCheck(f, args, 1, 3)
	str, ok := args[0].(slip.String)
	if !ok {
		slip.PanicType("string", args[0], "string")
	}
	if 1 < len(args) {
		var (
			start int
			end   int
		)
		ra := []rune(str)
		if num, ok := args[1].(slip.Fixnum); ok {
			start = int(num)
			if start < 0 || len(ra) <= start {
				slip.NewPanic("start, %d is outside the bounds of the string of length %d", start, len(ra))
			}
		} else {
			slip.PanicType("start", args[1], "fixnum")
		}
		if 2 < len(args) && args[2] != nil {
			if num, ok := args[2].(slip.Fixnum); ok {
				end = int(num)
				if end < 0 || len(ra) < end {
					slip.NewPanic("end, %d is outside the bounds of the string of length %d", end, len(ra))
				}
				if end <= start {
					slip.NewPanic("end, %d is before start %d", end, start)
				}
			} else {
				slip.PanicType("end", args[2], "fixnum", "nil")
			}
		} else {
			end = len(ra)
		}
		str = slip.String(ra[start:end])
	}
	return slip.NewStringStream([]byte(str))
}
