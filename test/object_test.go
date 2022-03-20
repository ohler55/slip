// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"
	"time"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"

	"github.com/stretchr/testify/require"
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
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.Float(7.0), Expect: true},
			{Other: slip.Float(7.5), Expect: false},
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

func TestFloat(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Float(7.0),
		String:    "7",
		Simple:    float64(7.0),
		Hierarchy: "float.real.number.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.Float(7.0), Expect: true},
			{Other: slip.Float(7.5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Selfies: []func() slip.Symbol{
			slip.Float(0).RealType,
			slip.Float(0).NumberType,
		},
		Eval: slip.Float(7.0),
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

func TestList(t *testing.T) {
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
	require.Equal(t, "nil", slip.Cons{}.String())
	require.Equal(t, "(1)", slip.Cons{slip.Fixnum(1)}.String())
	require.Equal(t, "(1)", slip.Cons{nil, slip.Fixnum(1)}.String())
	require.Equal(t, "(1 . 2)", slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}.String())
	require.Equal(t, "(nil)", slip.Cons{nil}.String())
	require.Equal(t, "(nil . 1)", slip.Cons{slip.Fixnum(1), nil}.String())
	require.Equal(t, "(1 2 3 nil)", slip.Cons{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}.String())
}

func TestConsCar(t *testing.T) {
	require.Equal(t, "1", slip.ObjectString(slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}.Car()))
	require.Equal(t, "nil", slip.ObjectString(slip.Cons{}.Car()))
}

func TestConsCdr(t *testing.T) {
	require.Equal(t, "2", slip.ObjectString(slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}.Cdr()))
	require.Equal(t, "nil", slip.ObjectString(slip.Cons{slip.Fixnum(1)}.Cdr()))
	require.Equal(t, "(2 3)", slip.ObjectString(slip.Cons{slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}.Cdr()))
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

// TBD simple
// TBD file-stream
// TBD values
