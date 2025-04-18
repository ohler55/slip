// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"math"
	"math/big"
	"os"
	"strings"
	"syscall"
	"testing"
	"time"

	"github.com/ohler55/ojg/jp"
	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTrue(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.True,
		String:    "t",
		Simple:    true,
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: slip.True, Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
		},
		Eval: slip.True,
	}).Test(t)
}

func TestFixnum(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Fixnum(7),
		String:    "7",
		Simple:    int64(7),
		Hierarchy: "fixnum.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.NewRatio(7, 1), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
			{Other: slip.Octet(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.DoubleFloat(7.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
			{Other: slip.SingleFloat(7.0), Expect: true},
			{Other: slip.NewLongFloat(7.0), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.Fixnum(0).IntegerType,
			slip.Fixnum(0).RationalType,
			slip.Fixnum(0).RealType,
			slip.Fixnum(0).NumberType,
		},
		Eval: slip.Fixnum(7),
	}).Test(t)
	tt.Equal(t, 7.0, slip.Fixnum(7).RealValue())
	tt.Equal(t, 7, slip.Fixnum(7).Int64())
	tt.Equal(t, true, slip.Fixnum(7).IsInt64())
}

func TestOctet(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Octet(7),
		String:    "7",
		Simple:    int64(7),
		Hierarchy: "octet.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Octet(7), Expect: true},
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.NewRatio(7, 1), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
			{Other: slip.Octet(5), Expect: false},
			{Other: slip.DoubleFloat(7.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
			{Other: slip.SingleFloat(7.0), Expect: true},
			{Other: slip.NewLongFloat(7.0), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.Octet(0).IntegerType,
			slip.Octet(0).RationalType,
			slip.Octet(0).RealType,
			slip.Octet(0).NumberType,
		},
		Eval: slip.Octet(7),
	}).Test(t)
	tt.Equal(t, 7.0, slip.Octet(7).RealValue())
	tt.Equal(t, 7, slip.Octet(7).Int64())
}

func TestRatio(t *testing.T) {
	r := slip.NewRatio(1, 1)
	(&sliptest.Object{
		Target:    slip.NewRatio(12, 8),
		String:    "3/2",
		Simple:    1.5,
		Hierarchy: "ratio.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.NewRatio(9, 6), Expect: true},
			{Other: slip.NewRatio(5, 2), Expect: false},
			{Other: slip.DoubleFloat(1.5), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
			{Other: slip.SingleFloat(1.5), Expect: true},
			{Other: slip.NewLongFloat(1.5), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			r.RationalType,
			r.RealType,
			r.NumberType,
		},
		Eval: slip.NewRatio(3, 2),
	}).Test(t)
	(&sliptest.Object{
		Target:    slip.NewRatio(14, 2),
		String:    "7",
		Simple:    int64(7),
		Hierarchy: "ratio.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
		},
		Eval: slip.Fixnum(7),
	}).Test(t)
	(&sliptest.Object{
		Target:    slip.NewRatio(-21, 28),
		String:    "-3/4",
		Simple:    -0.75,
		Hierarchy: "ratio.rational.real.number.t",
		Eval:      slip.NewRatio(-3, 4),
	}).Test(t)
	(&sliptest.Object{
		Target:    slip.NewRatio(0, 7),
		String:    "0",
		Simple:    0,
		Hierarchy: "ratio.rational.real.number.t",
		Eval:      slip.NewRatio(0, 1),
	}).Test(t)
	(&sliptest.Object{
		Target:    slip.NewRatio(7, 9),
		String:    "7/9",
		Simple:    "7/9",
		Hierarchy: "ratio.rational.real.number.t",
		Eval:      slip.NewRatio(7, 9),
	}).Test(t)
	tt.Equal(t, 1.5, slip.NewRatio(12, 8).RealValue())

	tt.Panic(t, func() { _ = slip.NewRatio(7, 0) })
	tt.Panic(t, func() { _ = slip.NewBigRatio(big.NewInt(7), big.NewInt(0)) })
}

func TestBignum(t *testing.T) {
	b := (*slip.Bignum)(big.NewInt(0).Add(big.NewInt(math.MaxInt64), big.NewInt(math.MaxInt64)))
	(&sliptest.Object{
		Target:    slip.NewBignum(123),
		String:    "123",
		Simple:    123,
		Hierarchy: "bignum.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.NewBignum(123), Expect: true},
			{Other: slip.NewBignum(5), Expect: false},
			{Other: slip.DoubleFloat(123.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
			{Other: slip.SingleFloat(123.0), Expect: true},
			{Other: slip.NewLongFloat(123.0), Expect: true},
			{Other: slip.Fixnum(123), Expect: true},
			{Other: slip.Fixnum(7), Expect: false},
			{Other: slip.Octet(123), Expect: true},
			{Other: slip.NewRatio(123, 1), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			b.IntegerType,
			b.RationalType,
			b.RealType,
			b.NumberType,
		},
		Eval: slip.NewBignum(123),
	}).Test(t)
	(&sliptest.Object{
		Target:    b,
		String:    "18446744073709551614",
		Simple:    "18446744073709551614",
		Hierarchy: "bignum.integer.rational.real.number.t",
		Eval:      b,
	}).Test(t)
	tt.Equal(t, 123.0, slip.NewBignum(123).RealValue())
	tt.Equal(t, 123, slip.NewBignum(123).Int64())
	tt.Equal(t, true, slip.NewBignum(123).IsInt64())
}

func TestShortFloat(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.ShortFloat(7.0),
		String:    "7",
		Simple:    float64(7.0),
		Hierarchy: "single-float.float.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.ShortFloat(7.0), Expect: true},
			{Other: slip.ShortFloat(7.5), Expect: false},
			{Other: slip.DoubleFloat(7.0), Expect: true},
			{Other: slip.NewLongFloat(7.0), Expect: true},
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Octet(7), Expect: true},
			{Other: slip.NewRatio(7, 1), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.ShortFloat(0).FloatType,
			slip.ShortFloat(0).RealType,
			slip.ShortFloat(0).NumberType,
		},
		Eval: slip.ShortFloat(7.0),
	}).Test(t)
	tt.Equal(t, 7.0, slip.ShortFloat(7.0).RealValue())
}

func TestSingleFloat(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.SingleFloat(7.0),
		String:    "7",
		Simple:    float64(7.0),
		Hierarchy: "single-float.float.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.SingleFloat(7.0), Expect: true},
			{Other: slip.SingleFloat(7.5), Expect: false},
			{Other: slip.DoubleFloat(7.0), Expect: true},
			{Other: slip.NewLongFloat(7.0), Expect: true},
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Octet(7), Expect: true},
			{Other: slip.NewRatio(7, 1), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.SingleFloat(0).FloatType,
			slip.SingleFloat(0).RealType,
			slip.SingleFloat(0).NumberType,
		},
		Eval: slip.SingleFloat(7.0),
	}).Test(t)
	tt.Equal(t, 7.0, slip.SingleFloat(7.0).RealValue())
}

func TestDoubleFloat(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.DoubleFloat(7.0),
		String:    "7",
		Simple:    float64(7.0),
		Hierarchy: "double-float.float.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.DoubleFloat(7.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
			{Other: slip.SingleFloat(7.0), Expect: true},
			{Other: slip.NewLongFloat(7.0), Expect: true},
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Octet(7), Expect: true},
			{Other: slip.NewRatio(7, 1), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.DoubleFloat(0).FloatType,
			slip.DoubleFloat(0).RealType,
			slip.DoubleFloat(0).NumberType,
		},
		Eval: slip.DoubleFloat(7.0),
	}).Test(t)
	tt.Equal(t, 7.0, slip.DoubleFloat(7.0).RealValue())
}

func TestLongFloat(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.NewLongFloat(7.0),
		String:    "7",
		Simple:    "7",
		Hierarchy: "long-float.float.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.NewLongFloat(7.0), Expect: true},
			{Other: slip.NewLongFloat(7.5), Expect: false},
			{Other: slip.DoubleFloat(7.0), Expect: true},
			{Other: slip.SingleFloat(7.0), Expect: true},
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Octet(7), Expect: true},
			{Other: slip.NewRatio(7, 1), Expect: true},
			{Other: slip.NewBignum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.NewLongFloat(0.0).FloatType,
			slip.NewLongFloat(0.0).RealType,
			slip.NewLongFloat(0.0).NumberType,
		},
		Eval: slip.NewLongFloat(7.0),
	}).Test(t)
	tt.Equal(t, 7.0, slip.NewLongFloat(7.0).RealValue())
}

func TestComplex(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Complex(1 + 2i),
		String:    "#C(1 2)",
		Simple:    "1+2i",
		Hierarchy: "complex.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Complex(1 + 2i), Expect: true},
			{Other: slip.Complex(2 + 3i), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.Complex(0).NumberType,
		},
		Eval: slip.Complex(1 + 2i),
	}).Test(t)
}

func TestString(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.String("abc"),
		String:    `"abc"`,
		Simple:    "abc",
		Hierarchy: "string.vector.array.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.String("abc"), Expect: true},
			{Other: slip.String("ABC"), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.String("abc"),
	}).Test(t)
	tt.Equal(t, slip.Symbol("string"), slip.String("x").SequenceType())
	tt.Equal(t, 3, slip.String("abc").Length())
}

func TestSymbolKey(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Symbol(":abc"),
		String:    ":abc",
		Simple:    ":abc",
		Hierarchy: "symbol.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Symbol(":abc"), Expect: true},
			{Other: slip.Symbol(":ABC"), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.Symbol(":abc"),
	}).Test(t)
}

func TestSymbol(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Symbol("abc"),
		String:    "abc",
		Simple:    "abc",
		Hierarchy: "symbol.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Symbol("abc"), Expect: true},
			{Other: slip.Symbol("ABC"), Expect: true},
			{Other: slip.String("xyz"), Expect: false},
			{Other: slip.True, Expect: false},
		},
		PanicType: slip.Symbol("unbound-variable"),
	}).Test(t)
}

func TestTime(t *testing.T) {
	tm := time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)
	(&sliptest.Object{
		Target:    slip.Time(tm),
		String:    "@2022-04-01T00:00:00Z",
		Simple:    tm,
		Hierarchy: "time.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Time(tm), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.Time(time.Date(2022, time.April, 1, 0, 0, 0, 1, time.UTC)), Expect: false},
		},
		Eval: slip.Time(tm),
	}).Test(t)
	tt.Equal(t, true, slip.Time(tm).HasMethod(":describe"))
	tt.Equal(t, "time", slip.Time(tm).Class().Name())
	(&sliptest.Function{
		Source: `(send @2024-11-24T12:00:00Z :add 4455)`,
		Expect: "@2024-11-24T13:14:15Z",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send @2024-11-24T12:00:00Z :add t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source: `(send @2024-11-12T13:14:15.123456789Z :components)`,
		Expect: `(2024 11 12 13 14 15 123456789 "Tuesday")`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(send @2024-11-24T12:00:00Z :elapsed @2024-11-24T13:14:15Z)`,
		Expect: "4455",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send @2024-11-24T12:00:00Z :elapsed t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `(send @2024-11-11T11:11:11Z :unix)`,
		Expect: "1.731323471e+09",
	}).Test(t)
	(&sliptest.Function{
		Source: `(send @2024-11-11T11:11:11Z :unix :second)`,
		Expect: "1.731323471e+09",
	}).Test(t)
	(&sliptest.Function{
		Source: `(send @2024-11-11T11:11:11Z :unix :millisecond)`,
		Expect: "1.731323471e+12",
	}).Test(t)
	(&sliptest.Function{
		Source: `(send @2024-11-11T11:11:11Z :unix :microsecond)`,
		Expect: "1.731323471e+15",
	}).Test(t)
	(&sliptest.Function{
		Source: `(send @2024-11-11T11:11:11Z :unix :nanosecond)`,
		Expect: "1731323471000000000",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send @2024-11-11T11:11:11Z :unix :hour)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)

	(&sliptest.Function{
		Source: `(let ((out (make-string-output-stream)))
                  (send @2024-11-24T12:00:00Z :describe out)
                  (get-output-stream-string out))`,
		Expect: "/Returns a list of the time components/",
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send @2024-11-24T12:00:00Z :describe 7)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(send @2024-11-24T12:00:00Z :not-a-method)`,
		PanicType: slip.MethodErrorSymbol,
	}).Test(t)
}

func TestListObj(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3), nil},
		String:    "(1 2 3 nil)",
		Simple:    []any{int64(1), int64(2), int64(3), nil},
		Hierarchy: "list.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3), nil}, Expect: true},
			{Other: slip.List{slip.Fixnum(1), slip.Fixnum(2), nil, nil}, Expect: false},
			{Other: slip.List{slip.Fixnum(1), slip.Fixnum(2)}, Expect: false},
			{Other: slip.List{slip.Fixnum(1), slip.Tail{Value: slip.Fixnum(2)}}, Expect: false}, // cons
			{Other: slip.True, Expect: false},
		},
		PanicType: slip.Symbol("error"),
		Selfies: []func() slip.Symbol{
			slip.List{}.SequenceType,
		},
	}).Test(t)
	tt.Equal(t, slip.List{slip.Fixnum(2)}, slip.List{slip.Fixnum(1), slip.Fixnum(2)}.Cdr())
	tt.Equal(t, slip.List{slip.Fixnum(2), slip.Fixnum(3)},
		slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Fixnum(3)}.Cdr())
	tt.Equal(t, 2, slip.List{slip.True, nil}.Length())
}

func TestListObjEmpty(t *testing.T) {
	(&sliptest.Object{
		Target: slip.List{},
		String: "nil",
		Simple: []any{},
	}).Test(t)
}

func TestCons(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.List{nil, slip.Tail{Value: slip.Fixnum(1)}},
		String:    "(nil . 1)",
		Simple:    []any{nil, int64(1)},
		Hierarchy: "cons.list.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.List{nil, slip.Tail{Value: slip.Fixnum(1)}}, Expect: true},
			{Other: slip.List{nil, slip.Fixnum(1)}, Expect: false},
			{Other: slip.List{slip.Fixnum(1), nil}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		PanicType: slip.Symbol("error"),
		Selfies: []func() slip.Symbol{
			slip.List{slip.Tail{}}.SequenceType,
		},
	}).Test(t)
	tt.Equal(t, slip.Fixnum(2), slip.List{slip.Fixnum(1), slip.Tail{Value: slip.Fixnum(2)}}.Cdr())
}

func TestConsString(t *testing.T) {
	tt.Equal(t, "(1 . 2)", slip.List{slip.Fixnum(1), slip.Tail{Value: slip.Fixnum(2)}}.String())
	tt.Equal(t, "(nil . 1)", slip.List{nil, slip.Tail{Value: slip.Fixnum(1)}}.String())
	tt.Equal(t, "(1 2 . 3)", slip.List{slip.Fixnum(1), slip.Fixnum(2), slip.Tail{Value: slip.Fixnum(3)}}.String())
}

func TestConsCar(t *testing.T) {
	tt.Equal(t, "1", slip.ObjectString(slip.List{slip.Fixnum(1), slip.Tail{Value: slip.Fixnum(2)}}.Car()))
}

func TestConsCdr(t *testing.T) {
	tt.Equal(t, "2", slip.ObjectString(slip.List{slip.Fixnum(1), slip.Tail{Value: slip.Fixnum(2)}}.Cdr()))
}

func TestCharacterUnicode(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Character('ぴ'),
		String:    `#\ぴ`,
		Simple:    "ぴ",
		Hierarchy: "character.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Character('ぴ'), Expect: true},
			{Other: slip.Character('x'), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.Character('ぴ'),
	}).Test(t)
}

func TestCharacterSpecial(t *testing.T) {
	(&sliptest.Object{
		Target: slip.Character(' '),
		String: `#\Space`,
		Simple: " ",
		Eval:   slip.Character(' '),
	}).Test(t)
}

func TestCharacterControl(t *testing.T) {
	(&sliptest.Object{
		Target: slip.Character('\u001b'),
		String: `#\u001b`,
		Simple: "\u001b",
		Eval:   slip.Character('\u001b'),
	}).Test(t)
}

func TestReadCharacter(t *testing.T) {
	tt.Equal(t, slip.Character('A'), slip.ReadCharacter([]byte{'A'}))
	tt.Equal(t, slip.Character(' '), slip.ReadCharacter([]byte("space")))
	tt.Equal(t, slip.Character('\u001b'), slip.ReadCharacter([]byte("u001b")))
	tt.Equal(t, slip.Character('ぴ'), slip.ReadCharacter([]byte("ぴ")))
	tt.Panic(t, func() { slip.ReadCharacter([]byte{}) })
	tt.Panic(t, func() { slip.ReadCharacter([]byte("u00112233")) })
}

func TestValues(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Values{nil, slip.Fixnum(2)},
		String:    "nil, 2",
		Simple:    []any{nil, int64(2)},
		Hierarchy: "values.t",
		Equals: []*sliptest.EqTest{
			// always false
			{Other: slip.Values{slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
	tt.Nil(t, slip.Values{nil, slip.Fixnum(2)}.First())
}

func TestSimpleObject(t *testing.T) {
	tm := time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)
	simple := []any{
		true,
		-1,
		int8(-2),
		int16(-3),
		int32(-4),
		int64(-5),
		uint(1),
		uint8(2),
		uint16(3),
		uint32(4),
		uint64(5),
		float32(4.5),
		5.4,
		tm,
		slip.True,
		"abc",
		[]byte("def"),
		fmt.Errorf("dummy error"),
		map[string]any{"x": 7},
	}
	obj := slip.SimpleObject(simple)
	tt.Equal(t,
		`(t -1 -2 -3 -4 -5 1 2 3 4 5 4.5 5.4 @2022-04-01T00:00:00Z t "abc" "def" "dummy error" (("x" . 7)))`,
		obj.String())

	p := slip.NewError("")
	p.Value = slip.Fixnum(7)
	obj = slip.SimpleObject(p)
	tt.Equal(t, "7", obj.String())

	p = slip.NewError("sample")
	obj = slip.SimpleObject(p)
	tt.Equal(t, `"sample"`, obj.String())
}

func TestSimple2(t *testing.T) {
	(&sliptest.Object{
		Target: &slip.Simple{Data: []any{nil, true}},
		String: "[null true]",
		Simple: []any{nil, true},
		Eval:   &slip.Simple{Data: []any{nil, true}},
		Equals: []*sliptest.EqTest{
			{Other: &slip.Simple{Data: []any{nil, true}}, Expect: true},
			{Other: slip.True, Expect: false},
		},
	}).Test(t)
}

func TestFileStream(t *testing.T) {
	(&sliptest.Object{
		Target:    (*slip.FileStream)(os.Stdout),
		String:    "/^#<FILE-STREAM /dev/stdout \\{[0-9+]\\}>$/",
		Simple:    "#<FILE-STREAM /dev/stdout {1}>",
		Hierarchy: "file-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: (*slip.FileStream)(os.Stdout), Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(*slip.FileStream)(os.Stdout).StreamType,
		},
		Eval: (*slip.FileStream)(os.Stdout),
	}).Test(t)
}

func TestFileStreamFile(t *testing.T) {
	filename := "testdata/sample"
	defer func() { _ = os.Remove(filename) }()

	f, err := os.Create(filename)
	tt.Nil(t, err)
	fs := (*slip.FileStream)(f)
	tt.Equal(t, 0, fs.LastByte())
	_, err = f.WriteString("hello")
	tt.Nil(t, err)
	tt.Equal(t, 'o', fs.LastByte())
	tt.Equal(t, slip.Fixnum(5), fs.FileLength())
	pos, _ := fs.Seek(0, 1)
	tt.Equal(t, int64(5), pos)
}

func TestFileStreamWriteRead(t *testing.T) {
	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	lr := (*slip.FileStream)(pr)
	lw := (*slip.FileStream)(pw)

	_, _ = lw.Write([]byte("hello"))

	// Should not be able to get the last byte.
	tt.Equal(t, 0, lw.LastByte())

	tt.Equal(t, true, lw.IsOpen())
	_ = lw.Close()
	tt.Equal(t, false, lw.IsOpen())

	buf := make([]byte, 10)
	n, err := lr.Read(buf)
	tt.Nil(t, err)

	tt.Equal(t, "hello", string(buf[:n]))
}

type rwBuilder struct {
	strings.Builder
}

func (b *rwBuilder) Close() error {
	return nil
}

func (b *rwBuilder) Seek(offset int64, whence int) (int64, error) {
	var pos int64
	switch whence {
	case 0:
		pos = offset
	case 1, 2:
		pos = int64(len(b.String())) + offset
	}
	return pos, nil
}

func (b *rwBuilder) ReadAt(p []byte, off int64) (n int, err error) {
	buf := []byte(b.String())
	if off < 0 || int64(len(buf)) <= off {
		return -1, fmt.Errorf("out of range")
	}
	copy(p, buf[off:])
	n = len(p)
	if len(buf[off:]) < n {
		n = len(buf[off:])
	}
	return n, nil
}

func (b *rwBuilder) Read(p []byte) (n int, err error) {
	buf := []byte(b.String())
	copy(p, buf)
	n = len(p)
	if len(buf) < n {
		n = len(buf)
	}
	return n, nil
}

func TestOutputStream(t *testing.T) {
	var out rwBuilder
	stream := slip.OutputStream{Writer: &out}
	(&sliptest.Object{
		Target:    &stream,
		String:    "#<OUTPUT-STREAM>",
		Simple:    "#<OUTPUT-STREAM>",
		Hierarchy: "output-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: &stream, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&slip.OutputStream{}).StreamType,
		},
		Eval: &stream,
	}).Test(t)

	tt.Equal(t, 0, stream.LastByte())
	_, _ = stream.Write([]byte("hello"))
	tt.Equal(t, 'o', stream.LastByte())

	tt.Equal(t, true, stream.IsOpen())
	_ = stream.Close()
	_, err := stream.Write([]byte{'x'})
	tt.NotNil(t, err)
	tt.Equal(t, false, stream.IsOpen())
}

func TestIOStream(t *testing.T) {
	fds, _ := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	f0 := os.NewFile(uintptr(fds[0]), "file-0")
	f1 := os.NewFile(uintptr(fds[1]), "file-1")
	stream := slip.IOStream{RW: f0}
	stream1 := slip.IOStream{RW: f1}
	defer func() {
		_ = syscall.Close(fds[0])
		_ = syscall.Close(fds[1])
		_ = f0.Close()
		_ = f1.Close()
		_ = stream.Close()
		_ = stream1.Close()
	}()
	(&sliptest.Object{
		Target:    &stream,
		String:    "#<IO-STREAM>",
		Simple:    "#<IO-STREAM>",
		Hierarchy: "io-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: &stream, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&slip.IOStream{}).StreamType,
		},
		Eval: &stream,
	}).Test(t)
	cnt, err := stream.Write([]byte("test"))
	tt.Nil(t, err)
	tt.Equal(t, 4, cnt)

	buf := make([]byte, 4)
	cnt, err = stream1.Read(buf)
	tt.Nil(t, err)
	tt.Equal(t, 4, cnt)
	tt.Equal(t, "test", string(buf))

	tt.Equal(t, true, stream.IsOpen())
	_ = stream.Close()
	tt.Equal(t, false, stream.IsOpen())
}

func TestIOStream2(t *testing.T) {
	var out rwBuilder
	stream := slip.IOStream{RW: &out}
	_, err := stream.Write([]byte("hello"))
	tt.Nil(t, err)
	tt.Equal(t, 'o', stream.LastByte())
}

func TestSignedByteSmall(t *testing.T) {
	sb := &slip.SignedByte{
		Bytes: []byte{0x08, 0x02},
	}
	(&sliptest.Object{
		Target:    sb,
		String:    "2050",
		Simple:    int64(2050),
		Hierarchy: "signed-byte.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Fixnum(2050), Expect: true},
			{Other: slip.NewRatio(2050, 1), Expect: true},
			{Other: slip.NewBignum(2050), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.DoubleFloat(2050.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
		},
		Selfies: []func() slip.Symbol{
			sb.IntegerType,
			sb.RationalType,
			sb.RealType,
			sb.NumberType,
		},
		Eval: sb,
	}).Test(t)
	tt.Equal(t, 2050.0, sb.RealValue())
	tt.Equal(t, 2050, sb.Int64())
	tt.Equal(t, true, sb.IsInt64())
	tt.Equal(t, 16, sb.Size())

	sb = slip.SignedByteFromInt64(2050)
	tt.Equal(t, 2050, sb.Int64())
	sb = slip.SignedByteFromInt64(-2050)
	tt.Equal(t, -2050, sb.Int64())
	tt.Equal(t, true, sb.IsNeg())

	sb = slip.SignedByteFromUint64(2050)
	tt.Equal(t, 2050, sb.Int64())
}

func TestSignedByteBig(t *testing.T) {
	sb := &slip.SignedByte{
		Bytes: []byte{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
	}
	(&sliptest.Object{
		Target:    sb,
		String:    "2050",
		Simple:    int64(2050),
		Hierarchy: "signed-byte.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: sb, Expect: true},
			{Other: slip.NewBignum(2050), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.DoubleFloat(2050.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
		},
		Selfies: []func() slip.Symbol{
			sb.IntegerType,
			sb.RationalType,
			sb.RealType,
			sb.NumberType,
		},
		Eval: sb,
	}).Test(t)
	tt.Equal(t, 2050.0, sb.RealValue())
	tt.Equal(t, 2050, sb.Int64())
	tt.Equal(t, true, sb.IsInt64())

	sb = &slip.SignedByte{
		Bytes: []byte{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
	}
	tt.Equal(t, 2050, sb.Int64())

	sb = slip.SignedByteFromBigInt(big.NewInt(2050))
	sb.Neg()
	tt.Equal(t, -2050, sb.Int64())

	sb = slip.SignedByteFromBigInt(big.NewInt(0))
	tt.Equal(t, 0, sb.Int64())

	sb = &slip.SignedByte{
		Bytes: []byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
	}
	tt.Equal(t, "75557863725914323421186", sb.String())
	sb.Neg()
	tt.Equal(t, "-75557863725914323421186", sb.String())
	tt.Equal(t, true, sb.IsNeg())

	var bi big.Int
	_ = bi.SetBytes([]byte{0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02})
	_ = bi.Neg(&bi)
	sb = slip.SignedByteFromBigInt(&bi)
	tt.Equal(t, "-75557863725914323421186", sb.String())

	// Test carry on add 1.
	sb = &slip.SignedByte{Bytes: []byte{0x7f, 0xff}}
	tt.Equal(t, 32767, sb.Int64())
	sb.Neg()
	tt.Equal(t, -32767, sb.Int64())

	sb = &slip.SignedByte{Bytes: []byte{0x80, 0x00}}
	tt.Equal(t, -32768, sb.Int64())
	sb.Neg()
	tt.Equal(t, 32768, sb.Int64())
}

func TestUnsignedByteSmall(t *testing.T) {
	ub := slip.UnsignedByte{
		Bytes: []byte{0x08, 0x02},
	}
	(&sliptest.Object{
		Target:    &ub,
		String:    "2050",
		Simple:    int64(2050),
		Hierarchy: "unsigned-byte.signed-byte.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Fixnum(2050), Expect: true},
			{Other: slip.NewRatio(2050, 1), Expect: true},
			{Other: slip.NewBignum(2050), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.DoubleFloat(2050.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
		},
		Selfies: []func() slip.Symbol{
			ub.IntegerType,
			ub.RationalType,
			ub.RealType,
			ub.NumberType,
		},
		Eval: &ub,
	}).Test(t)
	tt.Equal(t, 2050.0, ub.RealValue())
	tt.Equal(t, 2050, ub.Int64())
	tt.Equal(t, true, ub.IsInt64())
	tt.Equal(t, 16, ub.Size())

	tt.Equal(t, false, ub.GetBit(0))
	tt.Equal(t, true, ub.GetBit(1))
	tt.Equal(t, false, ub.GetBit(10))
	tt.Equal(t, true, ub.GetBit(11))
	tt.Equal(t, false, ub.GetBit(16))

	dup := ub.Dup()
	tt.Equal(t, 2050, dup.Int64())
	dup.SetBit(17, true)
	dup.SetBit(16, false)
	tt.Equal(t, 2050, ub.Int64())
	tt.Equal(t, false, ub.GetBit(17))
	tt.Equal(t, true, dup.GetBit(17))
	tt.Equal(t, 133122, dup.Int64())

	ub.Invert()
	tt.Equal(t, 63485, ub.Int64())
}

func TestUnsignedByteBig(t *testing.T) {
	ub := slip.UnsignedByte{
		Bytes: []byte{0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x02},
	}
	(&sliptest.Object{
		Target:    &ub,
		String:    "2050",
		Simple:    int64(2050),
		Hierarchy: "unsigned-byte.signed-byte.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: &ub, Expect: true},
			{Other: slip.NewBignum(2050), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.DoubleFloat(2050.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
		},
		Selfies: []func() slip.Symbol{
			ub.IntegerType,
			ub.RationalType,
			ub.RealType,
			ub.NumberType,
		},
		Eval: &ub,
	}).Test(t)
	tt.Equal(t, 2050.0, ub.RealValue())
	tt.Equal(t, 2050, ub.Int64())
	tt.Equal(t, true, ub.IsInt64())
}

func TestBit(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Bit(1),
		String:    "1",
		Simple:    int64(1),
		Hierarchy: "bit.unsigned-byte.signed-byte.integer.rational.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Bit(1), Expect: true},
			{Other: slip.Bit(0), Expect: false},
			{Other: slip.NewRatio(1, 1), Expect: true},
			{Other: slip.NewBignum(1), Expect: true},
			{Other: slip.Fixnum(1), Expect: true},
			{Other: slip.DoubleFloat(1.0), Expect: true},
			{Other: slip.DoubleFloat(7.5), Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.Bit(0).IntegerType,
			slip.Bit(0).RationalType,
			slip.Bit(0).RealType,
			slip.Bit(0).NumberType,
		},
		Eval: slip.Bit(1),
	}).Test(t)
	tt.Equal(t, 0.0, slip.Bit(0).RealValue())
	tt.Equal(t, 0, slip.Bit(0).Int64())
	tt.Equal(t, true, slip.Bit(0).IsInt64())
	tt.Equal(t, "0", slip.Bit(0).String())
}

func TestFuncInfo(t *testing.T) {
	fi := slip.MustFindFunc("car")
	(&sliptest.Object{
		Target: fi,
		String: "#<function car>",
		Simple: func(t *testing.T, simple any) {
			tt.Equal(t, "car", jp.C("name").First(simple))
		},
		Hierarchy: "built-in.t",
		Equals: []*sliptest.EqTest{
			{Other: fi, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: fi,
	}).Test(t)
	result := fi.Apply(slip.NewScope(), slip.Read([]byte("('(1 2))"), slip.NewScope())[0].(slip.List), 0)
	tt.Equal(t, slip.Fixnum(1), result)
}

func TestFuncInfoDescribeBasic(t *testing.T) {
	fi := slip.MustFindFunc("car")
	out := fi.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `Lambda-List: (arg)
Return: object
Documentation:
  car returns the car if arg is a cons, the first element if arg is a list, and
  nil if arg is nil or an empty list.
Arguments:
  arg: [list|cons]
    The value to take the first element of.

Examples:
  (car nil) => nil
  (car '(a . b) => a
  (car '(a b c)) => a

`, string(out))
}

func TestFuncInfoDescribeOptions(t *testing.T) {
	_ = slip.CompileString(`(defun func-info-test ((x 3) &options y) (list x y))`, slip.NewScope())
	fi := slip.MustFindFunc("func-info-test")
	out := fi.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `Lambda-List: ((x 3) &options y)
Return: object
Arguments:
  x: [fixnum] = 3
  y: [object]

`, string(out))

	out = fi.Describe([]byte{}, 0, 80, true)
	tt.Equal(t, "Lambda-List: ((x 3) &options y)\n"+
		"Return: object\n"+
		"Arguments:\n"+
		"  \x1b[4mx\x1b[m: [fixnum] = 3\n"+
		"  \x1b[4my\x1b[m: [object]\n\n", string(out))
}

func TestNovalue(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Novalue,
		String:    "",
		Simple:    nil,
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Novalue, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
}

func TestUndefined(t *testing.T) {
	obj := slip.Undefined("test")
	(&sliptest.Object{
		Target:    obj,
		String:    "test",
		Simple:    nil,
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: obj, Expect: true},
			{Other: slip.True, Expect: false},
		},
		PanicType: slip.Symbol("undefined-function"),
	}).Test(t)
}

func TestUnbound(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Unbound,
		String:    "<unbound> 0x00",
		Simple:    nil,
		Hierarchy: "unbound",
		Equals: []*sliptest.EqTest{
			{Other: slip.Unbound, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.Unbound,
	}).Test(t)
}

func TestTail(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Tail{Value: slip.True},
		String:    "t",
		Simple:    true,
		Hierarchy: "t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Tail{Value: slip.True}, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.True,
	}).Test(t)
}

type simplyBad int

// String representation.
func (sb simplyBad) String() string {
	return "x"
}

// Append the object to a byte slice.
func (sb simplyBad) Append(b []byte) []byte {
	return []byte{'x'}
}

// Simplify the Object into simple go types of nil, bool, int64, float64,
// string, []any, map[string]any, or time.Time.
func (sb simplyBad) Simplify() any {
	panic("bad")
}

// Equal returns true if this Object and the other are equal in value.
func (sb simplyBad) Equal(other slip.Object) bool {
	return false
}

// Hierarchy returns the class hierarchy as symbols for the instance.
func (sb simplyBad) Hierarchy() []slip.Symbol {
	return []slip.Symbol{slip.Symbol("x")}
}

// Eval the object.
func (sb simplyBad) Eval(s *slip.Scope, depth int) slip.Object {
	return nil
}

func TestSlipTest(t *testing.T) {
	(&sliptest.Object{
		Target:    simplyBad(0),
		String:    "x",
		Simple:    fmt.Errorf("bad"),
		Hierarchy: "x",
		Eval:      nil,
	}).Test(t)
}
