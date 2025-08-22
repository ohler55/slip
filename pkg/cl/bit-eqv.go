// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BitEqv{Function: slip.Function{Name: "bit-eqv", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bit-eqv",
			Args: []*slip.DocArg{
				{
					Name: "bit-array1",
					Type: "bit-array",
				},
				{
					Name: "bit-array2",
					Type: "bit-array",
				},
				{Name: "&optional"},
				{
					Name: "opt-arg",
					Type: "bit-array",
					Text: "A bit array for the result.",
				},
			},
			Return: "bit-array",
			Text: `__bit-eqv__ returns an array with the same dimension as both _bit-array1_ and
_bit-array2_ that is a logical XNOR of _bit-array1_ and _bit-array2_. If _opt-arg_
is provided it is used as the result array.`,
			Examples: []string{
				"(bit-eqv #*1010 #*1100) => #*1001",
			},
		}, &slip.CLPkg)
}

// BitEqv represents the bit-eqv function.
type BitEqv struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BitEqv) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	a1, a2, r := bitOpArrays(s, f, args, depth)
	switch ba1 := a1.(type) {
	case *slip.Array:
		ra := r.(*slip.Array)
		e1 := ba1.Elements()
		e2 := a2.(*slip.Array).Elements()
		for i, v := range e1 {
			if v == e2[i] {
				ra.MajorSet(i, slip.Bit(1))
			} else {
				ra.MajorSet(i, slip.Bit(0))
			}
		}
		result = ra
	case *slip.BitVector:
		ba2 := a2.(*slip.BitVector)
		ra := r.(*slip.BitVector)
		for i, b := range ba1.Bytes {
			ra.Bytes[i] = ^(b ^ ba2.Bytes[i])
		}
		result = ra
	}
	return
}
