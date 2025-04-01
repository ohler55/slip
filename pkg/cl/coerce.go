// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"
	"unicode/utf8"

	"github.com/ohler55/slip"
)

const starSym = slip.Symbol("*")

func init() {
	slip.Define(
		func(args slip.List) slip.Object {
			f := Coerce{Function: slip.Function{Name: "coerce", Args: args}}
			f.Self = &f
			return &f
		},
		&slip.FuncDoc{
			Name: "coerce",
			Args: []*slip.DocArg{
				{Name: "object", Type: "object"},
				{
					Name: "type",
					Type: "symbol",
					Text: `A type specifier for the result.`,
				},
			},
			Return: "object",
			Text: `__coerce__ returns _object_ converted to _type_. A _type_ of _t_
always return _object_. Although not Common LISP standard other conversion are
supported when possible as indicated in the table where the vertical axis the
_object_ type and the horizontal axis the the _type_:

                |list
                | |string
                | | |vector
                | | | |octets (bytes)
                | | | | |bit-vector
                | | | | | |character
                | | | | | | |integer
                | | | | | | | |fixnum
                | | | | | | | | |octet (byte)
                | | | | | | | | | |bignum
                | | | | | | | | | | |float
                | | | | | | | | | | | |short-float
                | | | | | | | | | | | | |single-float
                | | | | | | | | | | | | | |double-float
                | | | | | | | | | | | | | | |long-float
                | | | | | | | | | | | | | | | |rational
                | | | | | | | | | | | | | | | | |ratio
                | | | | | | | | | | | | | | | | | |complex
                | | | | | | | | | | | | | | | | | | |symbol
                | | | | | | | | | | | | | | | | | | | |assoc
                | | | | | | | | | | | | | | | | | | | | |hash-table
                | | | | | | | | | | | | | | | | | | | | | |function
                | | | | | | | | | | | | | | | | | | | | | | |signed-byte
                | | | | | | | | | | | | | | | | | | | | | | | |unsigned-byte
  list          |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
  string        |x|x|x|x| | | | | | | | | | | | | | |x| | |x| | |
  vector        |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
  octets        |x|x|x|x|x| | | | | | | | | | | | | | | | | | | |
  bit-vector    |x| |x|x|x| |x|x|x|x| | | | | | | | | | | | |x|x|
  character     | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  integer       | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  fixnum        | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  octet         | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |x|x|
  float         | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
  ratio         | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
  complex       | | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | | | |
  symbol        |x|x|x|x| | | | | | | | | | | | | | |x| | |x| | |
  assoc         |x| | | | | | | | | | | | | | | | | | |x|x| | | |
  hash-table    | | | | | | | | | | | | | | | | | | | |x|x| | | |
  signed-byte   | | | | |x|x|x|x|x|x| | | | | | | | | | | | |x|x|
  unsigned-byte | | | | |x|x|x|x|x|x| | | | | | | | | | | | |x|x|

`,
			Examples: []string{
				"(coerce 4 'float) => 4.0",
			},
		}, &slip.CLPkg)
}

// Coerce represents the coerce function.
type Coerce struct {
	slip.Function
}

// Call the function with the arguments provided.
func (f *Coerce) Call(s *slip.Scope, args slip.List, depth int) (result slip.Object) {
	slip.ArgCountCheck(f, args, 2, 2)
	switch t1 := args[1].(type) {
	case slip.Symbol:
		switch t1 {
		case slip.ListSymbol:
			result = coerceToList(args[0])
		case slip.StringSymbol:
			result = coerceToString(args[0])
		case slip.VectorSymbol:
			result = coerceToVector(args[0])
		case slip.CharacterSymbol:
			result = coerceToChar(args[0])
		case slip.IntegerSymbol:
			result = coerceToInteger(args[0])
		case slip.FixnumSymbol:
			result = coerceToFixnum(args[0])
		case slip.OctetSymbol, slip.ByteSymbol:
			result = ToOctet(args[0])
		case slip.OctetsSymbol, slip.Symbol("bytes"):
			result = coerceToOctets(args[0])
		case slip.BignumSymbol:
			result = coerceToBignum(args[0])
		case slip.FloatSymbol:
			result = coerceToFloat(args[0])
		case slip.ShortFloatSymbol, slip.SingleFloatSymbol:
			result = coerceToSingleFloat(args[0])
		case slip.DoubleFloatSymbol:
			result = coerceToDoubleFloat(args[0])
		case slip.LongFloatSymbol:
			result = coerceToLongFloat(args[0])
		case slip.RationalSymbol:
			result = coerceToRational(args[0])
		case slip.RatioSymbol:
			result = coerceToRatio(args[0])
		case slip.ComplexSymbol:
			result = coerceToComplex(args[0])
		case slip.SymbolSymbol:
			result = coerceToSymbol(args[0])
		case slip.Symbol("assoc"):
			result = coerceToAssoc(args[0])
		case slip.HashTableSymbol:
			result = coerceToHashTable(args[0])
		case slip.FunctionSymbol:
			result = coerceToFunction(args[0])
		case slip.BitVectorSymbol:
			result = coerceToBitVector(args[0])
		case slip.SignedByteSymbol:
			result = coerceToSignedByte(args[0])
		case slip.UnsignedByteSymbol:
			result = coerceToUnsignedByte(args[0])
		case slip.BitSymbol:
			result = coerceToBit(args[0])
		default:
			slip.NewPanic("%s is not a valid coerce result type", args[1])
		}
	case slip.List:
		if len(t1) < 1 {
			slip.NewPanic("%s is not a valid coerce result type", t1)
		}
		switch t1[0] {
		case slip.VectorSymbol:
			result = coerceToVector(args[0], t1[1:]...)
		case slip.IntegerSymbol:
			result = coerceToInteger(args[0], t1[1:]...)
		case slip.FixnumSymbol:
			result = coerceToFixnum(args[0], t1[1:]...)
		case slip.BignumSymbol:
			result = coerceToBignum(args[0], t1[1:]...)
		case slip.FloatSymbol:
			result = coerceToFloat(args[0], t1[1:]...)
		case slip.ShortFloatSymbol, slip.SingleFloatSymbol:
			result = coerceToSingleFloat(args[0], t1[1:]...)
		case slip.DoubleFloatSymbol:
			result = coerceToDoubleFloat(args[0], t1[1:]...)
		case slip.LongFloatSymbol:
			result = coerceToLongFloat(args[0], t1[1:]...)
		case slip.RationalSymbol:
			result = coerceToRational(args[0], t1[1:]...)
		case slip.RatioSymbol:
			result = coerceToRatio(args[0], t1[1:]...)
		case slip.BitVectorSymbol:
			result = coerceToBitVector(args[0], t1[1:]...)
		case slip.SignedByteSymbol:
			result = coerceToSignedByte(args[0], t1[1:]...)
		case slip.UnsignedByteSymbol:
			result = coerceToUnsignedByte(args[0], t1[1:]...)
		default:
			slip.NewPanic("%s is not a valid coerce result type", t1)
		}
	default:
		if t1 == slip.True {
			result = args[0]
		} else {
			slip.NewPanic("%s is not a valid coerce result type", t1)
		}
	}
	return
}

func coerceToList(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, slip.List:
		result = ta
	case slip.String:
		ra := []rune(ta)
		list := make(slip.List, len(ra))
		for i, r := range ra {
			list[i] = slip.Character(r)
		}
		result = list
	case slip.Symbol:
		ra := []rune(ta)
		list := make(slip.List, len(ra))
		for i, r := range ra {
			list[i] = slip.Character(r)
		}
		result = list
	case slip.VectorLike:
		result = ta.AsList()
	default:
		coerceNotPossible(ta, "list")
	}
	return
}

func coerceToVector(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, slip.VectorLike:
		result = ta
	case slip.String:
		ra := []rune(ta)
		elements := make(slip.List, len(ra))
		for i, r := range ra {
			elements[i] = slip.Character(r)
		}
		result = slip.NewVector(len(elements), slip.CharacterSymbol, nil, elements, false)
	case slip.Symbol:
		ra := []rune(ta)
		elements := make(slip.List, len(ra))
		for i, r := range ra {
			elements[i] = slip.Character(r)
		}
		result = slip.NewVector(len(elements), slip.CharacterSymbol, nil, elements, false)
	case slip.List:
		result = slip.NewVector(len(ta), slip.TrueSymbol, nil, ta, true)
	default:
		coerceNotPossible(ta, "vector")
	}
	if 0 < len(mods) {
		vl := result.(slip.VectorLike)
		if mods[0] != starSym {
			vl.SetElementType(mods[0])
		}
		if 1 < len(mods) && mods[1] != starSym {
			num, ok := mods[1].(slip.Fixnum)
			if !ok || num < 0 {
				slip.PanicType("size", mods[1], "non-negative fixnum")
			}
			if int(num) != vl.Length() {
				slip.NewPanic("The length requested (%d) does not match the type restriction in %s", vl.Length(),
					append(slip.List{slip.VectorSymbol}, mods...).String())
			}
		}
	}
	return
}

func coerceToOctets(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, slip.Octets:
		result = ta
	case slip.VectorLike:
		list := ta.AsList()
		octs := make(slip.Octets, len(list))
		for i, r := range list {
			octs[i] = byte(ToOctet(r).(slip.Octet))
		}
		result = octs
	case slip.String:
		result = slip.Octets(ta)
	case slip.Symbol:
		result = slip.Octets(ta)
	case slip.Character:
		result = slip.Octets(string([]rune{rune(ta)}))
	case slip.List:
		octs := make(slip.Octets, len(ta))
		for i, v := range ta {
			octs[i] = byte(ToOctet(v).(slip.Octet))
		}
		result = octs
	default:
		coerceNotPossible(ta, "octets")
	}
	return
}

func coerceToString(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil:
		result = slip.String("")
	case slip.List:
		result = coerceListToString(ta)
	case slip.String:
		result = ta
	case slip.Symbol:
		result = slip.String(ta)
	case slip.Octets:
		result = slip.String(ta)
	case *slip.Vector:
		result = coerceListToString(ta.AsList())
	default:
		coerceNotPossible(ta, "string")
	}
	return
}

func coerceToChar(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = ta
	case slip.Octet:
		result = slip.Character(ta)
	case slip.Integer:
		code := ta.Int64()
		if 0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) {
			result = slip.Character(rune(code))
		} else {
			coerceNotPossible(ta, "character")
		}
	case slip.Real:
		num := ta.RealValue()
		code := int64(num)
		if num == float64(code) && 0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) {
			result = slip.Character(rune(code))
		} else {
			coerceNotPossible(ta, "character")
		}
	case slip.Complex:
		code := real(ta)
		if imag(ta) == 0.0 &&
			0.0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) && code == float64(int64(code)) {
			result = slip.Character(rune(code))
		} else {
			coerceNotPossible(ta, "character")
		}
	default:
		coerceNotPossible(ta, "character")
	}
	return
}

func coerceToInteger(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.Fixnum(ta)
	case slip.Integer:
		result = ta
	case *slip.LongFloat:
		bf := (*big.Float)(ta)
		if bf.IsInt() {
			var bi big.Int
			_, _ = bf.Int(&bi)
			result = (*slip.Bignum)(&bi)
		} else {
			coerceNotPossible(ta, "integer")
		}
	case slip.Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			coerceNotPossible(ta, "integer")
		}
	case slip.Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			coerceNotPossible(ta, "integer")
		}
	case *slip.BitVector:
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

func coerceToFixnum(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.Fixnum(ta)
	case slip.Fixnum:
		result = ta
	case slip.Octet:
		result = slip.Fixnum(ta)
	case slip.Integer:
		if ta.IsInt64() {
			result = slip.Fixnum(ta.Int64())
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case *slip.LongFloat:
		i64, acc := (*big.Float)(ta).Int64()
		if acc == 0 {
			result = slip.Fixnum(i64)
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case slip.Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case slip.Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			coerceNotPossible(ta, "fixnum")
		}
	case *slip.BitVector:
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

func coerceToBignum(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = (*slip.Bignum)(big.NewInt(int64(ta)))
	case slip.Fixnum:
		result = (*slip.Bignum)(big.NewInt(int64(ta)))
	case slip.Octet:
		result = (*slip.Bignum)(big.NewInt(int64(ta)))
	case *slip.Bignum:
		result = ta
	case *slip.LongFloat:
		var bi big.Int
		_, acc := (*big.Float)(ta).Int(&bi)
		if acc == 0 {
			result = (*slip.Bignum)(&bi)
		} else {
			coerceNotPossible(ta, "bignum")
		}
	case *slip.SignedByte:
		num := ta.AsFixOrBig()
		if bi, ok := num.(*slip.Bignum); ok {
			result = bi
		} else { // must be a fixnum
			result = (*slip.Bignum)(big.NewInt(int64(num.(slip.Fixnum))))
		}
	case *slip.UnsignedByte:
		num := ta.AsFixOrBig()
		if bi, ok := num.(*slip.Bignum); ok {
			result = bi
		} else { // must be a fixnum
			result = (*slip.Bignum)(big.NewInt(int64(num.(slip.Fixnum))))
		}
	case slip.Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = (*slip.Bignum)(big.NewInt(int64(num)))
		} else {
			coerceNotPossible(ta, "bignum")
		}
	case slip.Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = (*slip.Bignum)(big.NewInt(int64(num)))
		} else {
			coerceNotPossible(ta, "bignum")
		}
	case *slip.BitVector:
		result = ta.AsBignum()
	default:
		coerceNotPossible(ta, "bignum")
	}
	if 0 < len(mods) {
		checkRange(result, mods)
	}
	return
}

func coerceToFloat(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.DoubleFloat(ta)
	case slip.Float:
		result = ta
	case slip.Real:
		result = slip.DoubleFloat(ta.RealValue())
	case slip.Complex:
		if imag(ta) == 0.0 {
			result = slip.DoubleFloat(real(ta))
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

func coerceToSingleFloat(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.SingleFloat(ta)
	case slip.SingleFloat:
		result = ta
	case slip.Real:
		result = slip.SingleFloat(ta.RealValue())
	case slip.Complex:
		if imag(ta) == 0.0 {
			result = slip.SingleFloat(real(ta))
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

func coerceToDoubleFloat(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.DoubleFloat(ta)
	case slip.DoubleFloat:
		result = ta
	case slip.Real:
		result = slip.DoubleFloat(ta.RealValue())
	case slip.Complex:
		if imag(ta) == 0.0 {
			result = slip.DoubleFloat(real(ta))
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

func coerceToLongFloat(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = (*slip.LongFloat)(big.NewFloat(float64(ta)))
	case *slip.LongFloat:
		result = ta
	case slip.Real:
		result = (*slip.LongFloat)(big.NewFloat(ta.RealValue()))
	case slip.Complex:
		if imag(ta) == 0.0 {
			result = (*slip.LongFloat)(big.NewFloat(real(ta)))
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

func coerceToRational(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.Fixnum(ta)
	case slip.Rational:
		result = ta
	case slip.Real:
		var rat big.Rat
		rat.SetFloat64(ta.RealValue())
		result = (*slip.Ratio)(&rat)
	case slip.Complex:
		if imag(ta) == 0 {
			result = slip.Fixnum(real(ta))
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

func coerceToRatio(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = (*slip.Ratio)(big.NewRat(int64(ta), 1))
	case *slip.Ratio:
		result = ta
	case slip.Integer:
		result = (*slip.Ratio)(big.NewRat(ta.Int64(), 1))
	case slip.Real:
		var rat big.Rat
		rat.SetFloat64(ta.RealValue())
		result = (*slip.Ratio)(&rat)
	case slip.Complex:
		if imag(ta) == 0 {
			result = (*slip.Ratio)(big.NewRat(int64(real(ta)), 1))
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

func coerceToComplex(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.Complex(complex(float64(ta), 0))
	case slip.Real:
		result = slip.Complex(complex(ta.RealValue(), 0))
	case slip.Complex:
		result = ta
	default:
		coerceNotPossible(ta, "complex")
	}
	return
}

func coerceToSymbol(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.String:
		result = slip.Symbol(ta)
	case slip.Symbol:
		result = ta
	default:
		coerceNotPossible(ta, "symbol")
	}
	return
}

func coerceToAssoc(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.List:
		for _, e := range ta {
			if e == nil {
				continue
			}
			if c, ok := e.(slip.List); !ok || len(c) < 2 {
				coerceNotPossible(ta, "assoc")
			}
		}
		result = ta
	case slip.HashTable:
		list := make(slip.List, 0, len(ta))
		for k, v := range ta {
			list = append(list, slip.List{k, slip.Tail{Value: v}})
		}
		result = list
	default:
		coerceNotPossible(ta, "assoc")
	}
	return
}

func coerceToHashTable(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.List:
		ht := slip.HashTable{}
		for _, e := range ta {
			if e == nil {
				continue
			}
			if c, ok := e.(slip.List); !ok || len(c) < 2 {
				coerceNotPossible(ta, "hash-table")
			} else {
				ht[c[0]] = c.Cdr()
			}
		}
		result = ht
	case slip.HashTable:
		result = ta
	default:
		coerceNotPossible(ta, "hash-table")
	}
	return
}

func coerceToFunction(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Symbol:
		result = slip.MustFindFunc(string(ta))
	case *slip.Lambda:
		result = ta
	default:
		coerceNotPossible(ta, "function")
	}
	return
}

func coerceToBitVector(arg slip.Object, mods ...slip.Object) (result slip.Object) {
top:
	switch ta := arg.(type) {
	case *slip.BitVector:
		result = ta
	case *slip.Bignum:
		ba := (*big.Int)(ta).Bytes()
		bv := slip.BitVector{
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
	case *slip.SignedByte:
		arg = ta.AsFixOrBig()
		goto top
	case *slip.UnsignedByte:
		arg = ta.AsFixOrBig()
		goto top
	case slip.Integer:
		bv := slip.BitVector{
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
	case slip.List:
		bv := slip.BitVector{
			Bytes:     make([]byte, len(ta)+1),
			Len:       uint(len(ta)),
			FillPtr:   -1,
			CanAdjust: true,
		}
		for i, v := range ta {
			bv.Set(v, i)
		}
		result = &bv
	case slip.VectorLike:
		arg = ta.AsList()
		goto top
	default:
		coerceNotPossible(ta, "bit-vector")
	}
	if 0 < len(mods) {
		num, ok := mods[0].(slip.Fixnum)
		if !ok || num < 0 {
			slip.PanicType("size", mods[0], "non-negative fixnum")
		}
		bv := result.(*slip.BitVector)
		if int(num) != int(bv.Len) {
			slip.NewPanic("The length requested (%d) does not match the type restriction in %s", bv.Len,
				append(slip.List{slip.BitVectorSymbol}, mods...).String())
		}
	}
	return
}

func coerceToSignedByte(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case *slip.SignedByte:
		result = ta
	case *slip.BitVector:
		ba := make([]byte, len(ta.Bytes))
		last := int(ta.Len) - 1
		for i := 0; i <= last; i++ {
			if ta.At(uint(i)) {
				j := last - i
				ba[j/8] |= 1 << (j % 8)
			}
		}
		result = &slip.SignedByte{
			Bytes: ba,
			Size:  ta.Len,
		}
	case *slip.Bignum:
		result = &slip.SignedByte{
			Bytes: (*big.Int)(ta).Bytes(),
			Size:  uint((*big.Int)(ta).BitLen()),
			Neg:   (*big.Int)(ta).Sign() < 0,
		}
	case *slip.UnsignedByte:
		b := make([]byte, len(ta.Bytes))
		copy(b, ta.Bytes)
		result = &slip.SignedByte{
			Bytes: b,
			Size:  ta.Size,
		}
	case slip.Integer:
		i64 := ta.Int64()
		var neg bool
		if i64 < 0 {
			neg = true
			i64 = -i64
		}
		result = &slip.SignedByte{
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
			Size: 64,
			Neg:  neg,
		}
	default:
		coerceNotPossible(ta, "signed-byte")
	}
	if 0 < len(mods) {
		if mods[0] != starSym {
			num, ok := mods[0].(slip.Fixnum)
			if !ok || num < 0 {
				slip.PanicType("size", mods[0], "non-negative fixnum")
			}
			sb := result.(*slip.SignedByte)
			if int(num) != int(sb.Size) {
				slip.NewPanic("The length requested (%d) does not match the type restriction in %s", sb.Size,
					append(slip.List{slip.SignedByteSymbol}, mods...).String())
			}
		}
	}
	return
}

func coerceToUnsignedByte(arg slip.Object, mods ...slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case *slip.UnsignedByte:
		result = ta
	case *slip.BitVector:
		ba := make([]byte, len(ta.Bytes))
		last := int(ta.Len) - 1
		for i := 0; i <= last; i++ {
			if ta.At(uint(i)) {
				j := last - i
				ba[j/8] |= 1 << (j % 8)
			}
		}
		result = &slip.UnsignedByte{
			Bytes: ba,
			Size:  ta.Len,
		}
	case *slip.Bignum:
		if (*big.Int)(ta).Sign() < 0 {
			coerceNotPossible(ta, "unsigned-byte")
		}
		result = &slip.UnsignedByte{
			Bytes: (*big.Int)(ta).Bytes(),
			Size:  uint((*big.Int)(ta).BitLen()),
		}
	case *slip.SignedByte:
		if ta.Neg {
			coerceNotPossible(ta, "unsigned-byte")
		}
		b := make([]byte, len(ta.Bytes))
		copy(b, ta.Bytes)
		result = &slip.SignedByte{
			Bytes: b,
			Size:  ta.Size,
		}
	case slip.Integer:
		i64 := ta.Int64()
		if i64 < 0 {
			coerceNotPossible(ta, "unsigned-byte")
		}
		result = &slip.UnsignedByte{
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
			Size: 64,
		}
	default:
		coerceNotPossible(ta, "unsigned-byte")
	}
	if 0 < len(mods) {
		if mods[0] != starSym {
			num, ok := mods[0].(slip.Fixnum)
			if !ok || num < 0 {
				slip.PanicType("size", mods[0], "non-negative fixnum")
			}
			ub := result.(*slip.UnsignedByte)
			if int(num) != int(ub.Size) {
				slip.NewPanic("The length requested (%d) does not match the type restriction in %s", ub.Size,
					append(slip.List{slip.UnsignedByteSymbol}, mods...).String())
			}
		}
	}
	return
}

func coerceToBit(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Integer:
		i64 := ta.Int64()
		switch i64 {
		case 0:
			result = slip.Bit(byte(0))
		case 1:
			result = slip.Bit(byte(1))
		default:
			coerceNotPossible(arg, "integer")
		}
	default:
		coerceNotPossible(arg, "integer")
	}
	return
}

func coerceListToString(list slip.List) slip.Object {
	ra := make([]rune, len(list))
	for i, v := range list {
		if r, ok := v.(slip.Character); ok {
			ra[i] = rune(r)
		} else {
			coerceNotPossible(list, "string")
		}
	}
	return slip.String(ra)
}

func coerceNotPossible(arg slip.Object, rtype string) {
	if arg == nil {
		slip.NewPanic("Can not coerce a nil into a %s", rtype)
	}
	slip.NewPanic("Can not coerce %s a %s into a %s", arg, arg.Hierarchy()[0], rtype)
}

func checkRange(v slip.Object, mods slip.List) {
	if mods[0] != starSym {
		if lessThan(v, mods[0]) {
			coerceNotPossible(v, append(slip.List{slip.IntegerSymbol}, mods...).String())
		}
	}
	if 1 < len(mods) {
		if mods[1] != starSym {
			if lessThan(mods[1], v) {
				coerceNotPossible(v, append(slip.List{slip.IntegerSymbol}, mods...).String())
			}
		}
	}
}
