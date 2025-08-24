// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BitAnd{Function: slip.Function{Name: "bit-and", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bit-and",
			Args: []*slip.DocArg{
				{
					Name: "bit-array1",
					Type: "bit-array",
					Text: "A bit array for a logical AND.",
				},
				{
					Name: "bit-array2",
					Type: "bit-array",
					Text: "A bit array for a logical AND.",
				},
				{Name: "&optional"},
				{
					Name: "opt-arg",
					Type: "bit-array",
					Text: "A bit array for the result of a logical AND.",
				},
			},
			Return: "bit-array",
			Text: `__bit-and__ returns an array with the same dimension as both _bit-array1_ and
_bit-array2_ that is a logical AND of _bit-array1_ and _bit-array2_. If _opt-arg_ is provided it
is used as the result array.`,
			Examples: []string{
				"(bit-and #*1010 #*1100) => #*1000",
			},
		}, &slip.CLPkg)
}

// BitAnd represents the bit-and function.
type BitAnd struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BitAnd) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	a1, a2, r := bitOpArrays(s, f, args, depth)
	switch ba1 := a1.(type) {
	case *slip.Array:
		ra := r.(*slip.Array)
		e1 := ba1.Elements()
		e2 := a2.(*slip.Array).Elements()
		for i, v := range e1 {
			if v == slip.Bit(1) && e2[i] == slip.Bit(1) {
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
			ra.Bytes[i] = b & ba2.Bytes[i]
		}
		result = ra
	}
	return
}
