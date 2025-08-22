// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := ByteSize{Function: slip.Function{Name: "byte-size", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "byte-size",
			Args: []*slip.DocArg{
				{
					Name: "bytespec",
					Type: "cons",
				},
			},
			Return: "fixnum",
			Text:   `__byte-size__ returns the size of a bytespec cons.`,
			Examples: []string{
				"(byte-size '(16 . 7)) => 7",
			},
		}, &slip.CLPkg)
}

// ByteSize represents the byte-size function.
type ByteSize struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *ByteSize) Call(s *slip.Scope, args slip.List, depth int) slip.Object {
	slip.CheckArgCount(s, depth, f, args, 1, 1)
	spec, ok := args[0].(slip.List)
	if !ok || len(spec) != 2 {
		slip.TypePanic(s, depth, "bytespec", args[0], "cons")
	}
	var size slip.Fixnum
	if size, ok = spec[0].(slip.Fixnum); !ok {
		slip.TypePanic(s, depth, "size", spec[0], "fixnum")
	}
	return size
}
