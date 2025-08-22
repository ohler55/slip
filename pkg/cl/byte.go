// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Byte{Function: slip.Function{Name: "byte", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "byte",
			Args: []*slip.DocArg{
				{
					Name: "size",
					Type: "fixnum",
				},
				{
					Name: "position",
					Type: "fixnum",
				},
			},
			Return: "cons",
			Text:   `__byte__ returns a bytespec which is a cons of the _size_ and _position_.`,
			Examples: []string{
				"(byte 16 7) => (16 . 7)",
			},
		}, &slip.CLPkg)
}

// Byte represents the byte function.
type Byte struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Byte) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 2, 2)
	size, ok := args[0].(slip.Fixnum)
	if !ok {
		slip.TypePanic(s, depth, "size", args[0], "fixnum")
	}
	var pos slip.Fixnum
	if pos, ok = args[1].(slip.Fixnum); !ok {
		slip.TypePanic(s, depth, "position", args[1], "fixnum")
	}
	return slip.List{size, slip.Tail{Value: pos}}
}
