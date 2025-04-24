// Copyright (c) 2025, Peter Ohler, All rights reserved.

package slip

import (
	"math/big"
	"unicode/utf8"
)

const starSym = Symbol("*")

// Coerce_ returns object converted to type. A type of t always returns
// object. Although not Common LISP standard other conversion are supported
// when possible as indicated in the table where the vertical axis the object
// type and the horizontal axis the type:
//
//               |list
//               | |string
//               | | |vector
//               | | | |octets (bytes)
//               | | | | |bit-vector
//               | | | | | |character
//               | | | | | | |integer
//               | | | | | | | |fixnum
//               | | | | | | | | |octet (byte)
//               | | | | | | | | | |bignum
//               | | | | | | | | | | |float
//               | | | | | | | | | | | |short-float
//               | | | | | | | | | | | | |single-float
//               | | | | | | | | | | | | | |double-float
//               | | | | | | | | | | | | | | |long-float
//               | | | | | | | | | | | | | | | |rational
//               | | | | | | | | | | | | | | | | |ratio
//               | | | | | | | | | | | | | | | | | |complex
//               | | | | | | | | | | | | | | | | | | |symbol
//               | | | | | | | | | | | | | | | | | | | |assoc
//               | | | | | | | | | | | | | | | | | | | | |hash-table
//               | | | | | | | | | | | | | | | | | | | | | |function
//               | | | | | | | | | | | | | | | | | | | | | | |signed-byte
//               | | | | | | | | | | | | | | | | | | | | | | | |unsigned-byte
// list          |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
// string        |x|x|x|x| | | | | | | | | | | | | | |x| | |x| | |
// vector        |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
// octets        |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
// bit-vector    |x| |x|x|x| |x|x|x|x| | | | | | | | | | | | |x|x|
// character     | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
// integer       | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
// fixnum        | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
// octet         | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
// float         | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
// ratio         | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
// complex       | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
// symbol        |x|x|x|x| | | | | | | | | | | | | | |x| | |x| | |
// assoc         |x| | | | | | | | | | | | | | | | | | |x|x| | | |
// hash-table    | | | | | | | | | | | | | | | | | | | |x|x| | | |
// signed-byte   | | | | |x|x|x|x|x|x| | | | | | | | | | | | |x|x|
// unsigned-byte | | | | |x|x|x|x|x|x| | | | | | | | | | | | |x|x|
func Coerce(object, typeSpec Object) (result Object) {
	switch t1 := typeSpec.(type) {
	case nil:
		result = object
	case Symbol:
		switch t1 {
		case TrueSymbol:
			result = object
		case ListSymbol:
			result = CoerceToList(object)
		case StringSymbol:
			result = CoerceToString(object)
		case VectorSymbol:
			result = CoerceToVector(object)
		case CharacterSymbol:
			result = coerceToChar(object)
		case IntegerSymbol:
			result = coerceToInteger(object)
		case FixnumSymbol:
			result = coerceToFixnum(object)
		case OctetSymbol, ByteSymbol:
			result = ToOctet(object)
		case OctetsSymbol, Symbol("bytes"):
			result = CoerceToOctets(object)
		case BignumSymbol:
			result = coerceToBignum(object)
		case FloatSymbol:
			result = coerceToFloat(object)
		case ShortFloatSymbol, SingleFloatSymbol:
			result = coerceToSingleFloat(object)
		case DoubleFloatSymbol:
			result = coerceToDoubleFloat(object)
		case LongFloatSymbol:
			result = coerceToLongFloat(object)
		case RationalSymbol:
			result = coerceToRational(object)
		case RatioSymbol:
			result = coerceToRatio(object)
		case ComplexSymbol:
			result = coerceToComplex(object)
		case SymbolSymbol:
			result = coerceToSymbol(object)
		case Symbol("assoc"):
			result = coerceToAssoc(object)
		case HashTableSymbol:
			result = coerceToHashTable(object)
		case FunctionSymbol:
			result = coerceToFunction(object)
		case BitVectorSymbol:
			result = coerceToBitVector(object)
		case SignedByteSymbol:
			result = coerceToSignedByte(object)
		case UnsignedByteSymbol:
			result = coerceToUnsignedByte(object)
		case BitSymbol:
			result = coerceToBit(object)
		default:
			NewPanic("%s is not a valid coerce result type", object)
		}
	case List:
		if len(t1) < 1 {
			NewPanic("%s is not a valid coerce result type", t1)
		}
		switch t1[0] {
		case VectorSymbol:
			result = CoerceToVector(object, t1[1:]...)
		case IntegerSymbol:
			result = coerceToInteger(object, t1[1:]...)
		case FixnumSymbol:
			result = coerceToFixnum(object, t1[1:]...)
		case BignumSymbol:
			result = coerceToBignum(object, t1[1:]...)
		case FloatSymbol:
			result = coerceToFloat(object, t1[1:]...)
		case ShortFloatSymbol, SingleFloatSymbol:
			result = coerceToSingleFloat(object, t1[1:]...)
		case DoubleFloatSymbol:
			result = coerceToDoubleFloat(object, t1[1:]...)
		case LongFloatSymbol:
			result = coerceToLongFloat(object, t1[1:]...)
		case RationalSymbol:
			result = coerceToRational(object, t1[1:]...)
		case RatioSymbol:
			result = coerceToRatio(object, t1[1:]...)
		case BitVectorSymbol:
			result = coerceToBitVector(object, t1[1:]...)
		case SignedByteSymbol:
			result = coerceToSignedByte(object, t1[1:]...)
		case UnsignedByteSymbol:
			result = coerceToUnsignedByte(object, t1[1:]...)
		default:
			NewPanic("%s is not a valid coerce result type", t1)
		}
	default:
		if t1 == True {
			result = object
		} else {
			NewPanic("%s is not a valid coerce result type", t1)
		}
	}
	return
}

// CoerceToList coerce to a list or panic.
func CoerceToList(arg Object) (result Object) {
	switch ta := arg.(type) {
	case nil, List:
		result = ta
	case String:
		ra := []rune(ta)
		list := make(List, len(ra))
		for i, r := range ra {
			list[i] = Character(r)
		}
		result = list
	case Symbol:
		ra := []rune(ta)
		list := make(List, len(ra))
		for i, r := range ra {
			list[i] = Character(r)
		}
		result = list
	case VectorLike:
		result = ta.AsList()
	default:
		coerceNotPossible(ta, "list")
	}
	return
}

// CoerceToVector coerce to vector or panic
func CoerceToVector(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case nil, VectorLike:
		result = ta
	case String:
		ra := []rune(ta)
		elements := make(List, len(ra))
		for i, r := range ra {
			elements[i] = Character(r)
		}
		result = NewVector(len(elements), CharacterSymbol, nil, elements, false)
	case Symbol:
		ra := []rune(ta)
		elements := make(List, len(ra))
		for i, r := range ra {
			elements[i] = Character(r)
		}
		result = NewVector(len(elements), CharacterSymbol, nil, elements, false)
	case List:
		result = NewVector(len(ta), TrueSymbol, nil, ta, true)
	default:
		coerceNotPossible(ta, "vector")
	}
	if 0 < len(mods) {
		vl := result.(VectorLike)
		if mods[0] != starSym {
			vl.SetElementType(mods[0])
		}
		if 1 < len(mods) && mods[1] != starSym {
			num, ok := mods[1].(Fixnum)
			if !ok || num < 0 {
				PanicType("size", mods[1], "non-negative fixnum")
			}
			if int(num) != vl.Length() {
				NewPanic("The length requested (%d) does not match the type restriction in %s", vl.Length(),
					append(List{VectorSymbol}, mods...).String())
			}
		}
	}
	return
}

// CoerceToOctets coerces to an octets or panic.
func CoerceToOctets(arg Object) (result Object) {
	switch ta := arg.(type) {
	case nil, Octets:
		result = ta
	case VectorLike:
		list := ta.AsList()
		octs := make(Octets, len(list))
		for i, r := range list {
			octs[i] = byte(ToOctet(r).(Octet))
		}
		result = octs
	case String:
		result = Octets(ta)
	case Symbol:
		result = Octets(ta)
	case Character:
		result = Octets(string([]rune{rune(ta)}))
	case List:
		octs := make(Octets, len(ta))
		for i, v := range ta {
			octs[i] = byte(ToOctet(v).(Octet))
		}
		result = octs
	default:
		coerceNotPossible(ta, "octets")
	}
	return
}

// CoerceToString coerce to a string or panic.
func CoerceToString(arg Object) (result Object) {
	switch ta := arg.(type) {
	case nil:
		result = String("")
	case List:
		result = coerceListToString(ta)
	case String:
		result = ta
	case Symbol:
		result = String(ta)
	case Octets:
		result = String(ta)
	case *Vector:
		result = coerceListToString(ta.AsList())
	default:
		coerceNotPossible(ta, "string")
	}
	return
}

func coerceToChar(arg Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = ta
	case Octet:
		result = Character(ta)
	case Integer:
		code := ta.Int64()
		if 0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) {
			result = Character(rune(code))
		} else {
			coerceNotPossible(ta, "character")
		}
	case Real:
		num := ta.RealValue()
		code := int64(num)
		if num == float64(code) && 0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) {
			result = Character(rune(code))
		} else {
			coerceNotPossible(ta, "character")
		}
	case Complex:
		code := real(ta)
		if imag(ta) == 0.0 &&
			0.0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) && code == float64(int64(code)) {
			result = Character(rune(code))
		} else {
			coerceNotPossible(ta, "character")
		}
	default:
		coerceNotPossible(ta, "character")
	}
	return
}

func coerceToInteger(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = Fixnum(ta)
	case Integer:
		result = ta
	case *LongFloat:
		bf := (*big.Float)(ta)
		if bf.IsInt() {
			var bi big.Int
			_, _ = bf.Int(&bi)
			result = (*Bignum)(&bi)
		} else {
			coerceNotPossible(ta, "integer")
		}
	case Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = Fixnum(num)
		} else {
			coerceNotPossible(ta, "integer")
		}
	case Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = Fixnum(num)
		} else {
			coerceNotPossible(ta, "integer")
		}
	case *BitVector:
		if num, ok := ta.AsFixnum(); ok {
			result = num
		} else {
			result = ta.AsBignum()
		}
	default:
		coerceNotPossible(ta, "integer")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToFixnum(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = Fixnum(ta)
	case Fixnum:
		result = ta
	case Octet:
		result = Fixnum(ta)
	case Integer:
		if ta.IsInt64() {
			result = Fixnum(ta.Int64())
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case *LongFloat:
		i64, acc := (*big.Float)(ta).Int64()
		if acc == 0 {
			result = Fixnum(i64)
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = Fixnum(num)
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = Fixnum(num)
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case *BitVector:
		if num, ok := ta.AsFixnum(); ok {
			result = num
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	default:
		coerceNotPossible(ta, "fixnum")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToBignum(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = (*Bignum)(big.NewInt(int64(ta)))
	case Fixnum:
		result = (*Bignum)(big.NewInt(int64(ta)))
	case Octet:
		result = (*Bignum)(big.NewInt(int64(ta)))
	case *Bignum:
		result = ta
	case *LongFloat:
		var bi big.Int
		_, acc := (*big.Float)(ta).Int(&bi)
		if acc == 0 {
			result = (*Bignum)(&bi)
		} else {
			coerceNotPossible(ta, "bignum")
		}
	case *SignedByte:
		num := ta.AsFixOrBig()
		if bi, ok := num.(*Bignum); ok {
			result = bi
		} else { // must be a fixnum
			result = (*Bignum)(big.NewInt(int64(num.(Fixnum))))
		}
	case *UnsignedByte:
		num := ta.AsFixOrBig()
		if bi, ok := num.(*Bignum); ok {
			result = bi
		} else { // must be a fixnum
			result = (*Bignum)(big.NewInt(int64(num.(Fixnum))))
		}
	case Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = (*Bignum)(big.NewInt(int64(num)))
		} else {
			coerceNotPossible(ta, "bignum")
		}
	case Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = (*Bignum)(big.NewInt(int64(num)))
		} else {
			coerceNotPossible(ta, "bignum")
		}
	case *BitVector:
		result = ta.AsBignum()
	default:
		coerceNotPossible(ta, "bignum")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToFloat(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = DoubleFloat(ta)
	case Float:
		result = ta
	case Real:
		result = DoubleFloat(ta.RealValue())
	case Complex:
		if imag(ta) == 0.0 {
			result = DoubleFloat(real(ta))
		} else {
			coerceNotPossible(ta, "float")
		}
	default:
		coerceNotPossible(ta, "float")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToSingleFloat(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = SingleFloat(ta)
	case SingleFloat:
		result = ta
	case Real:
		result = SingleFloat(ta.RealValue())
	case Complex:
		if imag(ta) == 0.0 {
			result = SingleFloat(real(ta))
		} else {
			coerceNotPossible(ta, "single-float")
		}
	default:
		coerceNotPossible(ta, "single-float")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToDoubleFloat(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = DoubleFloat(ta)
	case DoubleFloat:
		result = ta
	case Real:
		result = DoubleFloat(ta.RealValue())
	case Complex:
		if imag(ta) == 0.0 {
			result = DoubleFloat(real(ta))
		} else {
			coerceNotPossible(ta, "double-float")
		}
	default:
		coerceNotPossible(ta, "double-float")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToLongFloat(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = (*LongFloat)(big.NewFloat(float64(ta)))
	case *LongFloat:
		result = ta
	case Real:
		result = (*LongFloat)(big.NewFloat(ta.RealValue()))
	case Complex:
		if imag(ta) == 0.0 {
			result = (*LongFloat)(big.NewFloat(real(ta)))
		} else {
			coerceNotPossible(ta, "long-float")
		}
	default:
		coerceNotPossible(ta, "long-float")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToRational(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = Fixnum(ta)
	case Rational:
		result = ta
	case Real:
		var rat big.Rat
		rat.SetFloat64(ta.RealValue())
		result = (*Ratio)(&rat)
	case Complex:
		if imag(ta) == 0 {
			result = Fixnum(real(ta))
		} else {
			coerceNotPossible(ta, "rational")
		}
	default:
		coerceNotPossible(ta, "rational")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToRatio(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = (*Ratio)(big.NewRat(int64(ta), 1))
	case *Ratio:
		result = ta
	case Integer:
		result = (*Ratio)(big.NewRat(ta.Int64(), 1))
	case Real:
		var rat big.Rat
		rat.SetFloat64(ta.RealValue())
		result = (*Ratio)(&rat)
	case Complex:
		if imag(ta) == 0 {
			result = (*Ratio)(big.NewRat(int64(real(ta)), 1))
		} else {
			coerceNotPossible(ta, "ratio")
		}
	default:
		coerceNotPossible(ta, "ratio")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToComplex(arg Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		result = Complex(complex(float64(ta), 0))
	case Real:
		result = Complex(complex(ta.RealValue(), 0))
	case Complex:
		result = ta
	default:
		coerceNotPossible(ta, "complex")
	}
	return
}

func coerceToSymbol(arg Object) (result Object) {
	switch ta := arg.(type) {
	case String:
		result = Symbol(ta)
	case Symbol:
		result = ta
	default:
		coerceNotPossible(ta, "symbol")
	}
	return
}

func coerceToAssoc(arg Object) (result Object) {
	switch ta := arg.(type) {
	case List:
		for _, e := range ta {
			if e == nil {
				continue
			}
			if c, ok := e.(List); !ok || len(c) < 2 {
				coerceNotPossible(ta, "assoc")
			}
		}
		result = ta
	case HashTable:
		list := make(List, 0, len(ta))
		for k, v := range ta {
			list = append(list, List{k, Tail{Value: v}})
		}
		result = list
	default:
		coerceNotPossible(ta, "assoc")
	}
	return
}

func coerceToHashTable(arg Object) (result Object) {
	switch ta := arg.(type) {
	case List:
		ht := HashTable{}
		for _, e := range ta {
			if e == nil {
				continue
			}
			if c, ok := e.(List); !ok || len(c) < 2 {
				coerceNotPossible(ta, "hash-table")
			} else {
				ht[c[0]] = c.Cdr()
			}
		}
		result = ht
	case HashTable:
		result = ta
	default:
		coerceNotPossible(ta, "hash-table")
	}
	return
}

func coerceToFunction(arg Object) (result Object) {
	switch ta := arg.(type) {
	case Symbol:
		result = MustFindFunc(string(ta))
	case *Lambda:
		result = ta
	default:
		coerceNotPossible(ta, "function")
	}
	return
}

func coerceToBitVector(arg Object, mods ...Object) (result Object) {
top:
	switch ta := arg.(type) {
	case *BitVector:
		result = ta
	case *Bignum:
		ba := (*big.Int)(ta).Bytes()
		bv := BitVector{
			Bytes:     make([]byte, len(ba)+1),
			Len:       0,
			FillPtr:   -1,
			CanAdjust: true,
		}
		var pos uint
		for i, b := range ba {
			for j := 7; 0 <= j; j-- {
				pos++
				if b&(0x01<<j) != 0 {
					if bv.Len == 0 {
						bv.Len = uint((len(ba)-i-1)*8 + j + 1)
						pos = 0
					}
					bv.Put(pos, true)
				}
			}
		}
		result = &bv
	case *SignedByte:
		arg = ta.AsFixOrBig()
		goto top
	case *UnsignedByte:
		arg = ta.AsFixOrBig()
		goto top
	case Integer:
		bv := BitVector{
			Bytes:     make([]byte, 8),
			Len:       0,
			FillPtr:   -1,
			CanAdjust: true,
		}
		i64 := ta.Int64()
		var pos uint
		for i := 63; 0 <= i; i-- {
			pos++
			if i64&(1<<i) != 0 {
				if bv.Len == 0 {
					bv.Len = uint(i + 1)
					pos = 0
				}
				bv.Put(pos, true)
			}
		}
		result = &bv
	case List:
		bv := BitVector{
			Bytes:     make([]byte, len(ta)+1),
			Len:       uint(len(ta)),
			FillPtr:   -1,
			CanAdjust: true,
		}
		for i, v := range ta {
			bv.Set(v, i)
		}
		result = &bv
	case VectorLike:
		arg = ta.AsList()
		goto top
	default:
		coerceNotPossible(ta, "bit-vector")
	}
	if 0 < len(mods) {
		num, ok := mods[0].(Fixnum)
		if !ok || num < 0 {
			PanicType("size", mods[0], "non-negative fixnum")
		}
		bv := result.(*BitVector)
		if int(num) != int(bv.Len) {
			NewPanic("The length requested (%d) does not match the type restriction in %s", bv.Len,
				append(List{BitVectorSymbol}, mods...).String())
		}
	}
	return
}

func coerceToSignedByte(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case *SignedByte:
		result = ta
	case *BitVector:
		ba := make([]byte, len(ta.Bytes))
		last := int(ta.Len) - 1
		for i := 0; i <= last; i++ {
			if ta.At(uint(i)) {
				j := last - i
				ba[j/8] |= 1 << (j % 8)
			}
		}
		result = &SignedByte{Bytes: ba}
	case *Bignum:
		result = SignedByteFromBigInt((*big.Int)(ta))
	case *UnsignedByte:
		b := make([]byte, len(ta.Bytes))
		copy(b, ta.Bytes)
		result = &SignedByte{Bytes: b}
	case Integer:
		result = SignedByteFromInt64(ta.Int64())
	default:
		coerceNotPossible(ta, "signed-byte")
	}
	if 0 < len(mods) {
		if mods[0] != starSym {
			num, ok := mods[0].(Fixnum)
			if !ok || num < 0 {
				PanicType("size", mods[0], "non-negative fixnum")
			}
			sb := result.(*SignedByte)
			mx := int64(1) << int(num)
			if sb.IsInt64() && (mx <= sb.Int64() || sb.Int64() <= -mx) {
				NewPanic("%s can't be converted to a %s", num,
					append(List{SignedByteSymbol}, mods...).String())
			}
		}
	}
	return
}

func coerceToUnsignedByte(arg Object, mods ...Object) (result Object) {
	switch ta := arg.(type) {
	case *UnsignedByte:
		result = ta
	case *BitVector:
		ba := make([]byte, len(ta.Bytes))
		last := int(ta.Len) - 1
		for i := 0; i <= last; i++ {
			if ta.At(uint(i)) {
				j := last - i
				ba[j/8] |= 1 << (j % 8)
			}
		}
		result = &UnsignedByte{Bytes: ba}
	case *Bignum:
		if (*big.Int)(ta).Sign() < 0 {
			coerceNotPossible(ta, "unsigned-byte")
		}
		result = &UnsignedByte{Bytes: (*big.Int)(ta).Bytes()}
	case *SignedByte:
		if ta.IsNeg() {
			coerceNotPossible(ta, "unsigned-byte")
		}
		b := make([]byte, len(ta.Bytes))
		copy(b, ta.Bytes)
		result = &SignedByte{Bytes: b}
	case Integer:
		i64 := ta.Int64()
		if i64 < 0 {
			coerceNotPossible(ta, "unsigned-byte")
		}
		result = &UnsignedByte{
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
	default:
		coerceNotPossible(ta, "unsigned-byte")
	}
	if 0 < len(mods) {
		if mods[0] != starSym {
			num, ok := mods[0].(Fixnum)
			if !ok || num < 0 {
				PanicType("size", mods[0], "non-negative fixnum")
			}
			ub := result.(*UnsignedByte)
			if ub.IsInt64() && (1<<int64(num)) <= ub.Int64() {
				NewPanic("%s can't be converted to a %s", num,
					append(List{UnsignedByteSymbol}, mods...).String())
			}
		}
	}
	return
}

func coerceToBit(arg Object) (result Object) {
	switch ta := arg.(type) {
	case Integer:
		i64 := ta.Int64()
		switch i64 {
		case 0:
			result = Bit(byte(0))
		case 1:
			result = Bit(byte(1))
		default:
			coerceNotPossible(arg, "integer")
		}
	default:
		coerceNotPossible(arg, "integer")
	}
	return
}

func coerceListToString(list List) Object {
	ra := make([]rune, len(list))
	for i, v := range list {
		if r, ok := v.(Character); ok {
			ra[i] = rune(r)
		} else {
			coerceNotPossible(list, "string")
		}
	}
	return String(ra)
}

func coerceNotPossible(arg Object, rtype string) {
	if arg == nil {
		NewPanic("Can not coerce a nil into a %s", rtype)
	}
	NewPanic("Can not coerce %s a %s into a %s", arg, arg.Hierarchy()[0], rtype)
}

func checkRange(v Object, mods List) {
	if mods[0] != starSym {
		if LessThan(v, mods[0]) {
			coerceNotPossible(v, append(List{IntegerSymbol}, mods...).String())
		}
	}
	if 1 < len(mods) {
		if mods[1] != starSym {
			if LessThan(mods[1], v) {
				coerceNotPossible(v, append(List{IntegerSymbol}, mods...).String())
			}
		}
	}
}

// ToOctet coerces a value into an octet if possible.
func ToOctet(arg Object) (result Object) {
	switch ta := arg.(type) {
	case Character:
		if ta < 256 {
			result = Octet(ta)
		}
	case Fixnum:
		if 0 <= ta && ta < 256 {
			result = Octet(ta)
		}
	case Octet:
		result = ta
	case *Bignum:
		if (*big.Int)(ta).IsInt64() {
			num := (*big.Int)(ta).Int64()
			if 0 <= num && num < 256 {
				result = Octet(num)
			}
		}
	case *LongFloat:
		i64, acc := (*big.Float)(ta).Int64()
		if acc == 0 && 0 <= i64 && i64 < 256 {
			result = Octet(i64)
		}
	case Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) && 0.0 <= num && num < 256.0 {
			result = Octet(num)
		}
	case Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) && 0 <= num && num < 256 {
			result = Octet(num)
		}
	}
	if result == nil {
		if arg == nil {
			NewPanic("Can not coerce a nil into a octet")
		}
		NewPanic("Can not coerce %s a %s into an octet", arg, arg.Hierarchy()[0])
	}
	return
}

// NormalizeNumber normalizes numbers to the highest precedence where
// precedence is (lowest to highest) fixnum, bignum, ratio, single-float,
// double-float, long-float, complex. signed-byte and unsigned-byte are
// converted to bignum.
func NormalizeNumber(v0, v1 Object) (n0, n1 Object) {
top:
	switch t0 := v0.(type) {
	case Fixnum:
		n1 = v1
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
		case Octet:
			n0 = t0
			n1 = Fixnum(t1)
		case SingleFloat:
			n0 = SingleFloat(t0)
		case DoubleFloat:
			n0 = DoubleFloat(t0)
		case *LongFloat:
			n0 = (*LongFloat)(big.NewFloat(float64(t0)))
		case *Bignum:
			n0 = (*Bignum)(big.NewInt(int64(t0)))
		case *Ratio:
			n0 = (*Ratio)(big.NewRat(int64(t0), 1))
		case Complex:
			n0 = Complex(complex(float64(t0), 0.0))
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = tt1
			case *Bignum:
				n0 = (*Bignum)(big.NewInt(int64(t0)))
				n1 = tt1
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = tt1
			case *Bignum:
				n0 = (*Bignum)(big.NewInt(int64(t0)))
				n1 = tt1
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case Octet:
		v0 = Fixnum(t0)
		goto top
	case SingleFloat:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = SingleFloat(t1)
		case SingleFloat:
			n0 = t0
			n1 = t1
		case DoubleFloat:
			n0 = DoubleFloat(t0)
			n1 = t1
		case *LongFloat:
			n0 = (*LongFloat)(big.NewFloat(float64(t0)))
			n1 = t1
		case *Bignum:
			n0 = t0
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = (SingleFloat)(f)
		case *Ratio:
			n0 = t0
			n1 = SingleFloat(t1.RealValue())
		case Complex:
			n0 = Complex(complex(float64(t0), 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = SingleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = SingleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case DoubleFloat:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = DoubleFloat(t1)
		case SingleFloat:
			n0 = t0
			n1 = DoubleFloat(t1)
		case DoubleFloat:
			n0 = t0
			n1 = t1
		case *LongFloat:
			n0 = (*LongFloat)(big.NewFloat(float64(t0)))
			n1 = t1
		case *Bignum:
			n0 = t0
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = (DoubleFloat)(f)
		case *Ratio:
			n0 = t0
			n1 = DoubleFloat(t1.RealValue())
		case Complex:
			n0 = Complex(complex(float64(t0), 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = DoubleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = DoubleFloat(tt1)
			case *Bignum:
				n0 = (*LongFloat)(big.NewFloat(float64(t0)))
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *LongFloat:
		n0 = t0
		switch t1 := v1.(type) {
		case Fixnum:
			var z big.Float
			z.SetInt64(int64(t1))
			n1 = (*LongFloat)(&z)
		case SingleFloat:
			n1 = (*LongFloat)(big.NewFloat(float64(t1)))
		case DoubleFloat:
			n1 = (*LongFloat)(big.NewFloat(float64(t1)))
		case *LongFloat:
			n1 = t1
		case *Bignum:
			var z big.Float
			z.SetInt((*big.Int)(t1))
			n1 = (*LongFloat)(&z)
		case *Ratio:
			n1 = (*LongFloat)(big.NewFloat(t1.RealValue()))
		case Complex:
			f, _ := (*big.Float)(t0).Float64()
			n0 = Complex(complex(f, 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*LongFloat)(big.NewFloat(float64(tt1)))
			case *Bignum:
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
			case *Bignum:
				var z big.Float
				_ = z.SetInt((*big.Int)(tt1))
				n1 = (*LongFloat)(&z)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *Bignum:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = (*Bignum)(big.NewInt(int64(t1)))
		case SingleFloat:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = SingleFloat(f)
			n1 = t1
		case DoubleFloat:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = DoubleFloat(f)
			n1 = t1
		case *LongFloat:
			var z big.Float
			n0 = (*LongFloat)(z.SetInt((*big.Int)(t0)))
			n1 = t1
		case *Bignum:
			n0 = t0
			n1 = t1
		case *Ratio:
			if (*big.Int)(t0).IsInt64() {
				n0 = (*Ratio)(big.NewRat((*big.Int)(t0).Int64(), 1))
				n1 = t1
			} else {
				var z big.Float
				n0 = (*LongFloat)(z.SetInt((*big.Int)(t0)))
				f, _ := (*big.Rat)(t1).Float64()
				n1 = (*LongFloat)(big.NewFloat(f))
			}
		case Complex:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t0)).Float64()
			n0 = Complex(complex(f, 0.0))
			n1 = t1
		case *SignedByte:
			n0 = t0
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*Bignum)(big.NewInt(int64(tt1)))
			case *Bignum:
				n1 = tt1
			}
		case *UnsignedByte:
			n0 = t0
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = (*Bignum)(big.NewInt(int64(tt1)))
			case *Bignum:
				n1 = tt1
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *Ratio:
		switch t1 := v1.(type) {
		case Fixnum:
			n0 = t0
			n1 = (*Ratio)(big.NewRat(int64(t1), 1))
		case SingleFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = SingleFloat(f)
			n1 = t1
		case DoubleFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = DoubleFloat(f)
			n1 = t1
		case *LongFloat:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = (*LongFloat)(big.NewFloat(f))
			n1 = t1
		case *Bignum:
			if (*big.Int)(t1).IsInt64() {
				n0 = t0
				n1 = (*Ratio)(big.NewRat((*big.Int)(t1).Int64(), 1))
			} else {
				var z big.Float
				n1 = (*LongFloat)(z.SetInt((*big.Int)(t1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*LongFloat)(&z0)
			}
		case *Ratio:
			n0 = t0
			n1 = t1
		case Complex:
			f, _ := (*big.Rat)(t0).Float64()
			n0 = Complex(complex(f, 0.0))
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = (*Ratio)(big.NewRat(int64(tt1), 1))
			case *Bignum:
				var z big.Float
				n1 = (*LongFloat)(z.SetInt((*big.Int)(tt1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*LongFloat)(&z0)
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n0 = t0
				n1 = (*Ratio)(big.NewRat(int64(tt1), 1))
			case *Bignum:
				var z big.Float
				n1 = (*LongFloat)(z.SetInt((*big.Int)(tt1)))
				prec := z.Prec()
				var z0 big.Float
				z0.SetPrec(prec)
				z0.SetRat((*big.Rat)(t0))
				n0 = (*LongFloat)(&z0)
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case Complex:
		n0 = t0
		switch t1 := v1.(type) {
		case Fixnum:
			n1 = Complex(complex(float64(t1), 0.0))
		case SingleFloat:
			n1 = Complex(complex(float64(t1), 0.0))
		case DoubleFloat:
			n1 = Complex(complex(float64(t1), 0.0))
		case *LongFloat:
			f, _ := (*big.Float)(t1).Float64()
			n1 = Complex(complex(f, 0.0))
		case *Bignum:
			var z big.Float
			f, _ := z.SetInt((*big.Int)(t1)).Float64()
			n1 = Complex(complex(f, 0.0))
		case *Ratio:
			f, _ := (*big.Rat)(t1).Float64()
			n1 = Complex(complex(f, 0.0))
		case Complex:
			n1 = t1
		case *SignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = Complex(complex(float64(tt1), 0.0))
			case *Bignum:
				var z big.Float
				f, _ := z.SetInt((*big.Int)(tt1)).Float64()
				n1 = Complex(complex(f, 0.0))
			}
		case *UnsignedByte:
			switch tt1 := t1.AsFixOrBig().(type) {
			case Fixnum:
				n1 = Complex(complex(float64(tt1), 0.0))
			case *Bignum:
				var z big.Float
				f, _ := z.SetInt((*big.Int)(tt1)).Float64()
				n1 = Complex(complex(f, 0.0))
			}
		default:
			PanicType("numbers", t1, "number")
		}
	case *SignedByte:
		switch tt0 := t0.AsFixOrBig().(type) {
		case Fixnum:
			v0 = tt0
			goto top
		case *Bignum:
			v0 = tt0
			goto top
		}
	case *UnsignedByte: // change to signed-byte
		switch tt0 := t0.AsFixOrBig().(type) {
		case Fixnum:
			v0 = tt0
			goto top
		case *Bignum:
			v0 = tt0
			goto top
		}
	default:
		PanicType("numbers", t0, "number")
	}
	return
}

// LessThan returns true if v0 is less than v1.
func LessThan(v0, v1 Object) bool {
	v0, v1 = NormalizeNumber(v0, v1)
	switch ta := v0.(type) {
	case Fixnum:
		if ta < v1.(Fixnum) {
			return true
		}
	case SingleFloat:
		if ta < v1.(SingleFloat) {
			return true
		}
	case DoubleFloat:
		if ta < v1.(DoubleFloat) {
			return true
		}
	case *LongFloat:
		if (*big.Float)(v1.(*LongFloat)).Cmp((*big.Float)(ta)) >= 0 {
			return true
		}
	case *Bignum:
		if (*big.Int)(v1.(*Bignum)).Cmp((*big.Int)(ta)) >= 0 {
			return true
		}
	case *Ratio:
		if (*big.Rat)(v1.(*Ratio)).Cmp((*big.Rat)(ta)) >= 0 {
			return true
		}
	default:
		PanicType("numbers", v0, "real")
	}
	return false
}
