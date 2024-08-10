// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl

import (
	"math/big"
	"unicode/utf8"

	"github.com/ohler55/slip"
)

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
             | | | |octets
             | | | | |character
             | | | | | |integer
             | | | | | | |fixnum
             | | | | | | | |octet
             | | | | | | | | |bignum
             | | | | | | | | | |float
             | | | | | | | | | | |short-float
             | | | | | | | | | | | |single-float
             | | | | | | | | | | | | |double-float
             | | | | | | | | | | | | | |long-float
             | | | | | | | | | | | | | | |rational
             | | | | | | | | | | | | | | | |ratio
             | | | | | | | | | | | | | | | | |complex
             | | | | | | | | | | | | | | | | | |symbol
             | | | | | | | | | | | | | | | | | | |assoc
             | | | | | | | | | | | | | | | | | | | |hash-table
             | | | | | | | | | | | | | | | | | | | | |function
  list       |x|x|x|x| | | | | | | | | | | | | | | | | |
  string     |x|x|x|x| | | | | | | | | | | | | |x| | |x|
  vector     |x|x|x|x| | | | | | | | | | | | | | | | | |
  octets     |x|x|x|x| | | | | | | | | | | | | | | | | |
  character  | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  integer    | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  fixnum     | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  octet      | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  float      | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  ratio      | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  complex    | | | | |x|x|x|x|x|x|x|x|x|x|x|x|x| | | | |
  symbol     |x|x|x|x| | | | | | | | | | | | | |x| | |x|
  assoc      |x| | | | | | | | | | | | | | | | | |x|x| |
  hash-table | | | | | | | | | | | | | | | | | | |x|x| |

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
	switch args[1] {
	case slip.True:
		result = args[0]
	case slip.Symbol("list"):
		result = f.toList(args[0])
	case slip.Symbol("string"):
		result = f.toString(args[0])
	case slip.Symbol("vector"):
		result = f.toVector(args[0])
	case slip.Symbol("character"):
		result = f.toChar(args[0])
	case slip.Symbol("integer"):
		result = f.toInteger(args[0])
	case slip.Symbol("fixnum"):
		result = f.toFixnum(args[0])
	case slip.Symbol("octet"):
		result = ToOctet(args[0])
	case slip.Symbol("octets"):
		result = f.toOctets(args[0])
	case slip.Symbol("bignum"):
		result = f.toBignum(args[0])
	case slip.Symbol("float"):
		result = f.toFloat(args[0])
	case slip.Symbol("short-float"), slip.Symbol("single-float"):
		result = f.toSingleFloat(args[0])
	case slip.Symbol("double-float"):
		result = f.toDoubleFloat(args[0])
	case slip.Symbol("long-float"):
		result = f.toLongFloat(args[0])
	case slip.Symbol("rational"):
		result = f.toRational(args[0])
	case slip.Symbol("ratio"):
		result = f.toRatio(args[0])
	case slip.Symbol("complex"):
		result = f.toComplex(args[0])
	case slip.Symbol("symbol"):
		result = f.toSymbol(args[0])
	case slip.Symbol("assoc"):
		result = f.toAssoc(args[0])
	case slip.Symbol("hash-table"):
		result = f.toHashTable(args[0])
	case slip.Symbol("function"):
		result = f.toFunction(args[0])
	default:
		slip.NewPanic("%s is not a valid coerce result type", args[1])
	}
	return
}

func (f *Coerce) toList(arg slip.Object) (result slip.Object) {
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
	case *slip.Vector:
		result = ta.AsList()
	case slip.Octets:
		result = ta.AsList()
	default:
		f.notPossible(ta, "list")
	}
	return
}

func (f *Coerce) toVector(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, *slip.Vector, slip.Octets:
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
		f.notPossible(ta, "vector")
	}
	return
}

func (f *Coerce) toOctets(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil, slip.Octets:
		result = ta
	case *slip.Vector:
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
		f.notPossible(ta, "octets")
	}
	return
}

func (f *Coerce) toString(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case nil:
		result = slip.String("")
	case slip.List:
		result = f.listToString(ta)
	case slip.String:
		result = ta
	case slip.Symbol:
		result = slip.String(ta)
	case slip.Octets:
		result = slip.String(ta)
	case *slip.Vector:
		result = f.listToString(ta.AsList())
	default:
		f.notPossible(ta, "string")
	}
	return
}

func (f *Coerce) toChar(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "character")
		}
	case slip.Real:
		num := ta.RealValue()
		code := int64(num)
		if num == float64(code) && 0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) {
			result = slip.Character(rune(code))
		} else {
			f.notPossible(ta, "character")
		}
	case slip.Complex:
		code := real(ta)
		if imag(ta) == 0.0 &&
			0.0 <= code && code <= utf8.MaxRune && utf8.ValidRune(rune(code)) && code == float64(int64(code)) {
			result = slip.Character(rune(code))
		} else {
			f.notPossible(ta, "character")
		}
	default:
		f.notPossible(ta, "character")
	}
	return
}

func (f *Coerce) toInteger(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "integer")
		}
	case slip.Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			f.notPossible(ta, "integer")
		}
	case slip.Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			f.notPossible(ta, "integer")
		}
	default:
		f.notPossible(ta, "integer")
	}
	return
}

func (f *Coerce) toFixnum(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.Fixnum(ta)
	case slip.Fixnum:
		result = ta
	case slip.Octet:
		result = slip.Fixnum(ta)
	case *slip.Bignum:
		if (*big.Int)(ta).IsInt64() {
			result = slip.Fixnum((*big.Int)(ta).Int64())
		} else {
			f.notPossible(ta, "fixnum")
		}
	case *slip.LongFloat:
		i64, acc := (*big.Float)(ta).Int64()
		if acc == 0 {
			result = slip.Fixnum(i64)
		} else {
			f.notPossible(ta, "fixnum")
		}
	case slip.Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			f.notPossible(ta, "fixnum")
		}
	case slip.Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = slip.Fixnum(num)
		} else {
			f.notPossible(ta, "fixnum")
		}
	default:
		f.notPossible(ta, "fixnum")
	}
	return
}

func (f *Coerce) toBignum(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "bignum")
		}
	case slip.Real: // other floats and ratio
		num := ta.RealValue()
		if num == float64(int64(num)) {
			result = (*slip.Bignum)(big.NewInt(int64(num)))
		} else {
			f.notPossible(ta, "bignum")
		}
	case slip.Complex:
		num := real(ta)
		if imag(ta) == 0.0 && num == float64(int64(num)) {
			result = (*slip.Bignum)(big.NewInt(int64(num)))
		} else {
			f.notPossible(ta, "bignum")
		}
	default:
		f.notPossible(ta, "bignum")
	}
	return
}

func (f *Coerce) toFloat(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "float")
		}
	default:
		f.notPossible(ta, "float")
	}
	return
}

func (f *Coerce) toSingleFloat(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "single-float")
		}
	default:
		f.notPossible(ta, "single-float")
	}
	return
}

func (f *Coerce) toDoubleFloat(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "double-float")
		}
	default:
		f.notPossible(ta, "double-float")
	}
	return
}

func (f *Coerce) toLongFloat(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "long-float")
		}
	default:
		f.notPossible(ta, "long-float")
	}
	return
}

func (f *Coerce) toRational(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "rational")
		}
	default:
		f.notPossible(ta, "rational")
	}
	return
}

func (f *Coerce) toRatio(arg slip.Object) (result slip.Object) {
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
			f.notPossible(ta, "ratio")
		}
	default:
		f.notPossible(ta, "ratio")
	}
	return
}

func (f *Coerce) toComplex(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Character:
		result = slip.Complex(complex(float64(ta), 0))
	case slip.Real:
		result = slip.Complex(complex(ta.RealValue(), 0))
	case slip.Complex:
		result = ta
	default:
		f.notPossible(ta, "complex")
	}
	return
}

func (f *Coerce) toSymbol(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.String:
		result = slip.Symbol(ta)
	case slip.Symbol:
		result = ta
	default:
		f.notPossible(ta, "symbol")
	}
	return
}

func (f *Coerce) toAssoc(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.List:
		for _, e := range ta {
			if e == nil {
				continue
			}
			if c, ok := e.(slip.List); !ok || len(c) < 2 {
				f.notPossible(ta, "assoc")
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
		f.notPossible(ta, "assoc")
	}
	return
}

func (f *Coerce) toHashTable(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.List:
		ht := slip.HashTable{}
		for _, e := range ta {
			if e == nil {
				continue
			}
			if c, ok := e.(slip.List); !ok || len(c) < 2 {
				f.notPossible(ta, "hash-table")
			} else {
				ht[c[0]] = c.Cdr()
			}
		}
		result = ht
	case slip.HashTable:
		result = ta
	default:
		f.notPossible(ta, "hash-table")
	}
	return
}

func (f *Coerce) toFunction(arg slip.Object) (result slip.Object) {
	switch ta := arg.(type) {
	case slip.Symbol:
		result = slip.MustFindFunc(string(ta))
	case *slip.Lambda:
		result = ta
	default:
		f.notPossible(ta, "function")
	}
	return
}

func (f *Coerce) listToString(list slip.List) slip.Object {
	ra := make([]rune, len(list))
	for i, v := range list {
		if r, ok := v.(slip.Character); ok {
			ra[i] = rune(r)
		} else {
			f.notPossible(list, "string")
		}
	}
	return slip.String(ra)
}

func (f *Coerce) notPossible(arg slip.Object, rtype string) {
	if arg == nil {
		slip.NewPanic("Can not coerce a nil into a %s", rtype)
	}
	slip.NewPanic("Can not coerce %s a %s into a %s", arg, arg.Hierarchy()[0], rtype)
}
