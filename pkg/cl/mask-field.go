// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := MaskField{Function: slip.Function{Name: "mask-field", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "mask-field",
			Args: []*slip.DocArg{
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
			Text: `__mask-field__ returns the a copy of _integer_ with all but the
_bytespec_ bit set to zero. Note that only __bignum__, __signed-byte__, and __unsigned-byte__
can be modified with _setf_.`,
			Examples: []string{
				"(mask-field (byte 3 1) 15) => 14",
			},
		}, &slip.CLPkg)
}

// MaskField represents the mask-field function.
type MaskField struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *MaskField) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	// Helper functions are defined in deposit-field.go.
	slip.ArgCountCheck(f, args, 2, 2)
	integer, _ := ToUnsignedByte(args[1], "integer")
	size, pos := byteSpecArg(args[0])

	ub := slip.UnsignedByte{Bytes: make([]byte, len(integer.Bytes))}
	for i := uint(0); i < uint(size); i++ {
		off := i + uint(pos)
		ub.SetBit(off, integer.GetBit(off))
	}
	return convertUnsignedByte(&ub, args[1], false)
}

// Place a value in the first position of a list or cons.
func (f *MaskField) Place(s *slip.Scope, args slip.List, value slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	size, pos := byteSpecArg(args[0])
	uv, _ := ToUnsignedByte(value, "setf value")

	switch ti := args[1].(type) {
	case *slip.Bignum:
		bi := (*big.Int)(ti)
		for i := 0; i < size; i++ {
			if uv.GetBit(uint(i)) {
				_ = bi.SetBit(bi, i+pos, 1)
			} else {
				_ = bi.SetBit(bi, i+pos, 0)
			}
		}
	case *slip.SignedByte:
		for i := uint(0); i < uint(size); i++ {
			ti.SetBit(i+uint(pos), uv.GetBit(i))
		}
	case *slip.UnsignedByte:
		for i := uint(0); i < uint(size); i++ {
			ti.SetBit(i+uint(pos), uv.GetBit(i))
		}
	default:
		slip.PanicType("integer", ti, "bitnum", "signed-byte", "unsigned-byte")
	}
}
