// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BytePosition{Function: slip.Function{Name: "byte-position", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "byte-position",
			Args: []*slip.DocArg{
				{
					Name: "bytespec",
					Type: "cons",
				},
			},
			Return: "fixnum",
			Text:   `__byte-position__ returns the position of a bytespec cons.`,
			Examples: []string{
				"(byte-position '(16 . 7)) => 7",
			},
		}, &slip.CLPkg)
}

// BytePosition represents the byte-position function.
type BytePosition struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BytePosition) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	spec, ok := args[0].(slip.List)
	if !ok || len(spec) != 2 {
		slip.TypePanic(s, depth, "bytespec", args[0], "cons")
	}
	var tail slip.Tail
	if tail, ok = spec[1].(slip.Tail); !ok {
		slip.TypePanic(s, depth, "bytespec", args[0], "cons")
	}
	var pos slip.Fixnum
	if pos, ok = tail.Value.(slip.Fixnum); !ok {
		slip.TypePanic(s, depth, "position", tail.Value, "fixnum")
	}
	return pos
}
