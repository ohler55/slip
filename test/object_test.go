// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"io/ioutil"
	"math"
	"math/big"
	"os"
	"strings"
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

	tt.Panic(t, func() { _ = slip.NewRatio(7, 0) })
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
		Panics: true,
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
}

func TestListObj(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.List{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)},
		String:    "(1 2 3 nil)",
		Simple:    []interface{}{int64(1), int64(2), int64(3), nil},
		Hierarchy: "list.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.List{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.List{nil, nil, slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.List{slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.Cons{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.Cons{nil, nil, slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Panics: true,
		Selfies: []func() slip.Symbol{
			slip.List{}.SequenceType,
		},
	}).Test(t)
}

func TestListObjEmpty(t *testing.T) {
	(&sliptest.Object{
		Target: slip.List{},
		String: "nil",
		Simple: []interface{}{},
	}).Test(t)
}

func TestCons(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Cons{slip.Fixnum(1), nil},
		String:    "(nil . 1)",
		Simple:    []interface{}{nil, int64(1)},
		Hierarchy: "cons.list.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Cons{slip.Fixnum(1), nil}, Expect: true},
			{Other: slip.Cons{nil, slip.Fixnum(1)}, Expect: false},
			{Other: slip.Cons{slip.Fixnum(1)}, Expect: false},
			{Other: slip.List{slip.Fixnum(1), nil}, Expect: true},
			{Other: slip.List{nil, slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Panics: true,
		Selfies: []func() slip.Symbol{
			slip.Cons{}.SequenceType,
		},
	}).Test(t)
}

func TestConsEmpty(t *testing.T) {
	(&sliptest.Object{
		Target: slip.Cons{},
		String: "nil",
		Simple: []interface{}{},
		Eval:   nil,
	}).Test(t)
}

func TestConsString(t *testing.T) {
	tt.Equal(t, "nil", slip.Cons{}.String())
	tt.Equal(t, "(1)", slip.Cons{slip.Fixnum(1)}.String())
	tt.Equal(t, "(1)", slip.Cons{nil, slip.Fixnum(1)}.String())
	tt.Equal(t, "(1 . 2)", slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}.String())
	tt.Equal(t, "(nil)", slip.Cons{nil}.String())
	tt.Equal(t, "(nil . 1)", slip.Cons{slip.Fixnum(1), nil}.String())
	tt.Equal(t, "(1 2 3 nil)", slip.Cons{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}.String())
}

func TestConsCar(t *testing.T) {
	tt.Equal(t, "1", slip.ObjectString(slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}.Car()))
	tt.Equal(t, "nil", slip.ObjectString(slip.Cons{}.Car()))
}

func TestConsCdr(t *testing.T) {
	tt.Equal(t, "2", slip.ObjectString(slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}.Cdr()))
	tt.Equal(t, "nil", slip.ObjectString(slip.Cons{slip.Fixnum(1)}.Cdr()))
	tt.Equal(t, "(2 3)", slip.ObjectString(slip.Cons{slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}.Cdr()))
}

func TestVector(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Vector{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)},
		String:    "#(1 2 3 nil)",
		Simple:    []interface{}{int64(1), int64(2), int64(3), nil},
		Hierarchy: "vector.array.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Vector{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.Vector{nil, nil, slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.Vector{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)},
		Selfies: []func() slip.Symbol{
			slip.Vector{}.SequenceType,
			slip.Vector{}.ArrayType,
		},
	}).Test(t)
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

func TestValues(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Values{slip.Fixnum(2), nil},
		String:    "nil, 2",
		Simple:    []interface{}{nil, int64(2)},
		Hierarchy: "values.t",
		Equals: []*sliptest.EqTest{
			// always false
			{Other: slip.Values{slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: nil,
	}).Test(t)
	tt.Nil(t, slip.Values{slip.Fixnum(2), nil}.First())
}

func TestSimpleObject(t *testing.T) {
	tm := time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)
	simple := []interface{}{
		true, -1, int8(-2), int16(-3), int32(-4), int64(-5),
		uint(1), uint8(2), uint16(3), uint32(4), uint64(5),
		float32(4.5), 5.4, tm, slip.True, "abc", []byte("def"),
		fmt.Errorf("dummy error"),
		map[string]interface{}{"x": 7},
	}
	obj := slip.SimpleObject(simple)
	tt.Equal(t,
		`(t -1 -2 -3 -4 -5 1 2 3 4 5 4.5 5.4 @2022-04-01T00:00:00Z t "abc" "def" "dummy error" (("x" . 7)))`,
		obj.String())
}

func TestSimple2(t *testing.T) {
	(&sliptest.Object{
		Target: &slip.Simple{Data: []interface{}{nil, true}},
		String: "[null true]",
		Simple: []interface{}{nil, true},
		Eval:   &slip.Simple{Data: []interface{}{nil, true}},
		Equals: []*sliptest.EqTest{
			{Other: &slip.Simple{Data: []interface{}{nil, true}}, Expect: true},
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

func TestFileStreamWriteRead(t *testing.T) {
	pr, pw, err := os.Pipe()
	tt.Nil(t, err)
	defer func() { _ = pw.Close(); _ = pr.Close() }()

	lr := (*slip.FileStream)(pr)
	lw := (*slip.FileStream)(pw)

	_, _ = lw.Write([]byte("hello"))
	lw.Close()

	buf := make([]byte, 10)
	n, err := lr.Read(buf)
	tt.Nil(t, err)

	tt.Equal(t, "hello", string(buf[:n]))
}

func TestInputStream(t *testing.T) {
	stream := slip.InputStream{Reader: strings.NewReader("abc")}
	(&sliptest.Object{
		Target:    &stream,
		String:    "#<INPUT-STREAM>",
		Simple:    "#<INPUT-STREAM>",
		Hierarchy: "input-stream.stream.t",
		Equals: []*sliptest.EqTest{
			{Other: &stream, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			(&slip.InputStream{}).StreamType,
		},
		Eval: &stream,
	}).Test(t)
	data, err := ioutil.ReadAll(&stream)
	tt.Nil(t, err)
	tt.Equal(t, "abc", string(data))
}

func TestOutputStream(t *testing.T) {
	var out strings.Builder
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
}

func TestFuncInfo(t *testing.T) {
	fi := slip.FindFunc("car")
	(&sliptest.Object{
		Target: fi,
		String: "#<function car>",
		Simple: func(t *testing.T, simple any) {
			tt.Equal(t, "car", jp.C("name").First(simple))
		},
		Hierarchy: "function.t",
		Equals: []*sliptest.EqTest{
			{Other: fi, Expect: true},
			{Other: slip.True, Expect: false},
		},
		Eval: fi,
	}).Test(t)
	result := fi.Apply(slip.NewScope(), slip.Read([]byte("('(1 2))"))[0].(slip.List), 0)
	tt.Equal(t, slip.Fixnum(1), result)
}

func TestFuncInfoDescribeBasic(t *testing.T) {
	fi := slip.FindFunc("car")
	out := fi.Describe([]byte{}, 0, 80, false)
	tt.Equal(t, `Lambda-List: (arg)
Return: object
Description:
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
	_ = slip.CompileString(`(defun func-info-test ((x 3) &options y) (list x y))`)
	fi := slip.FindFunc("func-info-test")
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
		Panics: true,
	}).Test(t)
}
