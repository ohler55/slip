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
	sliptest.TestObject(t, slip.True, "t", true, "t",
		[]*sliptest.EqTest{{Other: slip.True, Expect: true}, {Other: nil, Expect: false}})
}

func TestFixnum(t *testing.T) {
	sliptest.TestObject(t,
		slip.Fixnum(7),
		"7",
		int64(7),
		"fixnum.integer.rational.real.number.t",
		[]*sliptest.EqTest{
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.Float(7.0), Expect: true},
			{Other: slip.Float(7.5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		slip.Fixnum(0).IntegerType,
		slip.Fixnum(0).RationalType,
		slip.Fixnum(0).RealType,
		slip.Fixnum(0).NumberType,
	)
}

func TestFloat(t *testing.T) {
	sliptest.TestObject(t,
		slip.Float(7.0),
		"7",
		float64(7.0),
		"float.real.number.t",
		[]*sliptest.EqTest{
			{Other: slip.Fixnum(7), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.Float(7.0), Expect: true},
			{Other: slip.Float(7.5), Expect: false},
			{Other: slip.True, Expect: false},
		},
		slip.Float(0.0).RealType,
		slip.Float(0.0).NumberType,
	)
}

func TestString(t *testing.T) {
	sliptest.TestObject(t,
		slip.String("abc"),
		`"abc"`,
		"abc",
		"string.vector.array.sequence.t",
		[]*sliptest.EqTest{
			{Other: slip.String("abc"), Expect: true},
			{Other: slip.String("ABC"), Expect: false},
			{Other: slip.True, Expect: false},
		},
	)
}

func TestTime(t *testing.T) {
	tm := time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)
	sliptest.TestObject(t,
		slip.Time(tm),
		"@2022-04-01T00:00:00Z",
		tm,
		"time.t",
		[]*sliptest.EqTest{
			{Other: slip.Time(tm), Expect: true},
			{Other: slip.Fixnum(5), Expect: false},
			{Other: slip.Time(time.Date(2022, time.April, 1, 0, 0, 0, 1, time.UTC)), Expect: false},
		},
	)
}

func TestList(t *testing.T) {
	sliptest.TestObject(t,
		slip.List{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)},
		"(1 2 3 nil)",
		[]interface{}{int64(1), int64(2), int64(3), nil},
		"list.sequence.t",
		[]*sliptest.EqTest{
			{Other: slip.List{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.List{nil, nil, slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.List{slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.Cons{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.Cons{nil, nil, slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		slip.List{}.SequenceType,
	)
}

func TestCons(t *testing.T) {
	sliptest.TestObject(t,
		slip.Cons{slip.Fixnum(2), slip.Fixnum(1)},
		"(1 . 2)",
		[]interface{}{int64(1), int64(2)},
		"cons.list.sequence.t",
		[]*sliptest.EqTest{
			{Other: slip.Cons{slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.Cons{nil, slip.Fixnum(1)}, Expect: false},
			{Other: slip.Cons{slip.Fixnum(1)}, Expect: false},
			{Other: slip.List{slip.Fixnum(2), slip.Fixnum(1)}, Expect: true},
			{Other: slip.Cons{nil, slip.Fixnum(1)}, Expect: false},
			{Other: slip.True, Expect: false},
		},
		slip.Cons{}.SequenceType,
	)
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
