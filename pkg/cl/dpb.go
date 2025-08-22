// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Dpb{Function: slip.Function{Name: "dpb", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "dpb",
			Args: []*slip.DocArg{
				{
					Name: "newbyte",
					Type: "integer",
				},
				{
					Name: "bytespec",
					Type: "cons",
				},
				{
					Name: "integer",
					Type: "integer",
				},
			},
			Return: "integer",
			Text: `__dpb__ replaces the set of bits in the _integer_ with the bits
in _newbyte_ according to the _bytespec_. The bits in a copy of _integer_ described
by _bytespec_ is replaced with the bits in _newbyte_.`,
			Examples: []string{
				"(dpb 1 (byte 1 10) 0) => 1024",
				"(dpb -2 (byte 2 10) 0) => 2048",
				"(dpb 1 (byte 2 10) 2048) => 1024",
			},
		}, &slip.CLPkg)
}

// Dpb represents the dpb function.
type Dpb struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Dpb) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	// Helper functions are defined in deposit-field.go.
	slip.CheckArgCount(s, depth, f, args, 3, 3)
	newbyte, nneg := ToUnsignedByte(s, args[0], "newbyte", depth)
	integer, neg := ToUnsignedByte(s, args[2], "integer", depth)
	size, pos := byteSpecArg(s, args[1], depth)

	integer = integer.Dup()
	max := uint(newbyte.Size())

	for i := uint(0); i < uint(size); i++ {
		off := i + uint(pos)
		if max <= i {
			if nneg {
				integer.SetBit(off, true)
			} else {
				integer.SetBit(off, false)
			}
		} else {
			integer.SetBit(off, newbyte.GetBit(i))
		}
	}
	return convertUnsignedByte(integer, args[2], neg)
}
