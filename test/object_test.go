// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"fmt"
	"testing"
	"time"

	"github.com/ohler55/slip"

	"github.com/stretchr/testify/require"
)

func TestTrueString(t *testing.T) {
	require.Equal(t, "t", slip.True.String())
}

func TestTrueAppend(t *testing.T) {
	require.Equal(t, "t", string(slip.True.Append([]byte{})))
}

func TestFixnumString(t *testing.T) {
	require.Equal(t, "7", slip.Fixnum(7).String())
}

func TestFixnumAppend(t *testing.T) {
	require.Equal(t, "7", string(slip.Fixnum(7).Append([]byte{})))
}

func TestFloatString(t *testing.T) {
	require.Equal(t, "7.5", slip.Float(7.5).String())
}

func TestFloatAppend(t *testing.T) {
	require.Equal(t, "7.5", string(slip.Float(7.5).Append([]byte{})))
}

func TestStringString(t *testing.T) {
	require.Equal(t, `"abc"`, slip.String("abc").String())
}

func TestStringAppend(t *testing.T) {
	require.Equal(t, `"abc"`, string(slip.String("abc").Append([]byte{})))
}

func TestTimeString(t *testing.T) {
	require.Equal(t, "@2022-04-01T00:00:00Z", slip.Time(time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)).String())
}

func TestListString(t *testing.T) {
	require.Equal(t, "(1 2 3 nil)", slip.List{nil, slip.Fixnum(3), slip.Fixnum(2), slip.Fixnum(1)}.String())
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

func TestSimpleObject(t *testing.T) {
	obj := slip.SimpleObject([]interface{}{
		true, false,
		1, int8(2), int16(3), int32(4), int64(5),
		uint(6), uint8(7), uint16(8), uint32(9), uint64(10),
		"abc", []byte{'x', 'y', 'z'},
		time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC),
		slip.Fixnum(77),
		fmt.Errorf("error"),
	})
	require.Equal(t,
		`(t nil 1 2 3 4 5 6 7 8 9 10 "abc" "xyz" @2022-04-01T00:00:00Z 77 "error")`,
		slip.ObjectString(obj))
}

func TestObjectEqualFixnum(t *testing.T) {
	require.True(t, slip.ObjectEqual(slip.Fixnum(7), slip.Fixnum(7)))
	require.True(t, slip.ObjectEqual(slip.Fixnum(7), slip.Float(7.0)))
	require.False(t, slip.ObjectEqual(slip.Fixnum(7), slip.Fixnum(3)))
}

func TestObjectEqualFloat(t *testing.T) {
	require.True(t, slip.ObjectEqual(slip.Float(7.5), slip.Float(7.5)))
	require.True(t, slip.ObjectEqual(slip.Float(7.0), slip.Fixnum(7)))
	require.False(t, slip.ObjectEqual(slip.Float(7.0), slip.Float(3.0)))
}

func TestObjectEqualTime(t *testing.T) {
	tm := time.Date(2022, time.April, 1, 0, 0, 0, 0, time.UTC)
	require.True(t, slip.ObjectEqual(slip.Time(tm), slip.Time(tm)))
	require.False(t, slip.ObjectEqual(slip.Time(tm), slip.Fixnum(7)))
}
