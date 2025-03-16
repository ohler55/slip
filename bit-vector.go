// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"bytes"
)

// BitVectorSymbol is the symbol with a value of "bit-vector".
const BitVectorSymbol = Symbol("bit-vector")

func init() {
	DefConstant(BitVectorSymbol, BitVectorSymbol, `A _bit-vector_ is a vector of bits.`)
}

// BitVector represents an integer with a specific number of bits.
type BitVector struct {
	// Bytes in left to right order, the last byte includes padding on the low
	// bits if needed.
	Bytes []byte
	Size  uint
}

// String representation of the Object.
func (obj *BitVector) String() string {
	return string(obj.Append([]byte{}))
}

// Append a buffer with a representation of the Object.
func (obj *BitVector) Append(b []byte) []byte {
	b = append(b, '#', '*')
	last := int(obj.Size / 8)
	for i, bb := range obj.Bytes {
		if i < last {
			for i := 7; 0 <= i; i-- {
				if (bb>>i)&0x01 == 0 {
					b = append(b, '0')
				} else {
					b = append(b, '1')
				}
			}
		} else {
			end := int(obj.Size % 8)
			for i := 0; i < end; i++ {
				if (bb>>(7-i))&0x01 == 0 {
					b = append(b, '0')
				} else {
					b = append(b, '1')
				}
			}
			break
		}
	}
	return b
}

// Simplify the Object into an int64.
func (obj *BitVector) Simplify() any {
	return obj.String()
}

// Equal returns true if this Object and the other are equal in value.
func (obj *BitVector) Equal(other Object) (eq bool) {
	if bv, ok := other.(*BitVector); ok {
		eq = obj.Size == bv.Size && bytes.Equal(obj.Bytes, bv.Bytes)
	}
	return
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (obj *BitVector) Hierarchy() []Symbol {
	return []Symbol{BitVectorSymbol, VectorSymbol, ArraySymbol, SequenceSymbol, TrueSymbol}
}

// SequenceType() Symbol returns 'bit-vector.
func (obj *BitVector) SequenceType() Symbol {
	return BitVectorSymbol
}

// Eval returns self.
func (obj *BitVector) Eval(s *Scope, depth int) Object {
	return obj
}
