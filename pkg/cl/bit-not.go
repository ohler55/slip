// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := BitNot{Function: slip.Function{Name: "bit-not", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "bit-not",
			Args: []*slip.DocArg{
				{
					Name: "bit-array",
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
			Text: `__bit-not__ returns an array with the same dimension as _bit-array_ that
is a logical NOT (complement of) of _bit-array_. If _opt-arg_ is provided it is used as the result array.`,
			Examples: []string{
				"(bit-not #*1010) => #*0101",
			},
		}, &slip.CLPkg)
}

// BitNot represents the bit-not function.
type BitNot struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *BitNot) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 1, 2)
	var ok bool
	switch t1 := args[0].(type) {
	case *slip.Array:
		if t1.ElementType() != slip.BitSymbol {
			slip.PanicType("bit-array1", t1, "bit-array")
		}
		d1 := t1.Dimensions()
		var ra *slip.Array
		if 1 < len(args) {
			ra, ok = args[1].(*slip.Array)
			if !ok || ra.ElementType() != slip.BitSymbol {
				slip.PanicType("opt-arg", args[1], "bit-array")
			}
			dr := ra.Dimensions()
			if len(d1) != len(dr) {
				slip.NewPanic("%s and %s do not have the same dimensions.", t1, ra)
			}
			for i, d := range d1 {
				if dr[i] != d {
					slip.NewPanic("%s and %s do not have the same dimensions.", t1, ra)
				}
			}
		} else {
			ra = slip.NewArray(d1, slip.BitSymbol, slip.Bit(0), nil, false)
		}
		for i, v := range t1.Elements() {
			if v == slip.Bit(0) {
				ra.MajorSet(i, slip.Bit(1))
			} else {
				ra.MajorSet(i, slip.Bit(0))
			}
		}
		result = ra
	case *slip.BitVector:
		var ra *slip.BitVector
		if 1 < len(args) {
			ra, ok = args[1].(*slip.BitVector)
			if !ok {
				slip.PanicType("opt-arg", args[1], "bit-array")
			}
			if t1.Len != ra.Len {
				slip.NewPanic("%s and %s do not have the same dimensions.", t1, ra)
			}
		} else {
			ra = &slip.BitVector{
				Bytes:   make([]byte, len(t1.Bytes)),
				Len:     t1.Len,
				FillPtr: -1,
			}
		}
		for i, b := range t1.Bytes {
			ra.Bytes[i] = ^b
		}
		result = ra
	default:
		slip.PanicType("bit-array", t1, "bit-array")
	}
	return
}
