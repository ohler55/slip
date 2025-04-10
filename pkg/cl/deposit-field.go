// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl

import (
	"fmt"
	"math/big"

	"github.com/ohler55/slip"
)

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := DepositField{Function: slip.Function{Name: "deposit-field", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "deposit-field",
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
			Text: `__deposit-field__ replaces the set of bits in the _integer_ with the bits
in _newbyte_. The _newbytes_ replace the bits specified by the _bytespec_ in a copy of _integer_
which is returned. The lower bits starting at zero are replaced in _integer_`,
			Examples: []string{
				"(deposit-field 7 (byte 2 1) 0) => 6",
				"(deposit-field 0 (byte 2 1) -3) => -7",
			},
		}, &slip.CLPkg)
}

// DepositField represents the deposit-field function.
type DepositField struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *DepositField) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 3, 3)
	newbyte, _ := ToUnsignedByte(args[0], "newbyte")
	integer, neg := ToUnsignedByte(args[2], "integer")
	size, pos := byteSpecArg(args[1])

	integer = integer.Dup()
	max := uint(newbyte.Size())
	for i := uint(0); i < uint(size); i++ {
		off := i + uint(pos)
		if max <= off {
			break
		}
		integer.SetBit(i, newbyte.GetBit(off))
	}

	// TBD remove
	fmt.Printf("*** %s (%d %d) %s (neg %t)\n", newbyte, size, pos, integer, neg)

	switch args[2].(type) {
	case slip.Fixnum:
		switch {
		case integer.IsInt64():
			result = slip.Fixnum(integer.Int64())
		case neg:
			bytes := make([]byte, len(integer.Bytes))
			copy(bytes, integer.Bytes)
			result = &slip.SignedByte{
				Bytes: bytes,
				Neg:   true,
			}
		default:
			result = integer
		}
	case *slip.Bignum:
		var bi big.Int
		_ = bi.SetBytes(integer.Bytes)
		if neg {
			_ = bi.Neg(&bi)
		}
		result = (*slip.Bignum)(&bi)
	case *slip.SignedByte:
		bytes := make([]byte, len(integer.Bytes))
		copy(bytes, integer.Bytes)
		result = &slip.SignedByte{
			Bytes: bytes,
			Neg:   neg,
		}
	}
	return
}

// ToUnsignedByte converts the arg to an UnsignedByte or panics.
func ToUnsignedByte(arg slip.Object, name string) (ub *slip.UnsignedByte, neg bool) {
	switch ta := arg.(type) {
	case slip.Fixnum:
		i64 := ta.Int64()
		if i64 < 0 {
			neg = true
			i64 = -i64
		}
		ub = &slip.UnsignedByte{
			Bytes: []byte{
				byte(i64 >> 56),
				byte((i64 >> 48) & 0x00000000000000ff),
				byte((i64 >> 40) & 0x00000000000000ff),
				byte((i64 >> 32) & 0x00000000000000ff),
				byte((i64 >> 24) & 0x00000000000000ff),
				byte((i64 >> 16) & 0x00000000000000ff),
				byte((i64 >> 8) & 0x00000000000000ff),
				byte(i64 & 0x00000000000000ff),
			},
		}
	case *slip.Bignum:
		if (*big.Int)(ta).Sign() < 0 {
			neg = true
		}
		ub = &slip.UnsignedByte{Bytes: (*big.Int)(ta).Bytes()}
	case *slip.SignedByte:
		bytes := make([]byte, len(ta.Bytes))
		copy(bytes, ta.Bytes)
		ub = &slip.UnsignedByte{Bytes: bytes}
		neg = ta.Neg
	case *slip.UnsignedByte:
		ub = ta
	default:
		slip.PanicType(name, arg, "integer")
	}
	return
}

func byteSpecArg(arg slip.Object) (size, pos int) {
	spec, ok := arg.(slip.List)
	if !ok || len(spec) != 2 {
		slip.PanicType("bytespec", arg, "cons")
	}
	var num slip.Fixnum
	if num, ok = spec[0].(slip.Fixnum); ok {
		size = int(num)
	} else {
		slip.PanicType("size", spec[0], "fixnum")
	}
	var tail slip.Tail
	if tail, ok = spec[1].(slip.Tail); !ok {
		slip.PanicType("bytespec", arg, "cons")
	}
	if num, ok = tail.Value.(slip.Fixnum); ok {
		pos = int(num)
	} else {
		slip.PanicType("position", tail.Value, "fixnum")
	}
	return
}
