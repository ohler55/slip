// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
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

func TestIntegerString(t *testing.T) {
	require.Equal(t, "7", slip.Integer(7).String())
}

func TestIntegerAppend(t *testing.T) {
	require.Equal(t, "7", string(slip.Integer(7).Append([]byte{})))
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
	require.Equal(t, "(1 2 3 nil)", slip.List{nil, slip.Integer(3), slip.Integer(2), slip.Integer(1)}.String())
}

func TestConsString(t *testing.T) {
	require.Equal(t, "nil", slip.Cons{}.String())
	require.Equal(t, "(1)", slip.Cons{slip.Integer(1)}.String())
	require.Equal(t, "(1)", slip.Cons{nil, slip.Integer(1)}.String())
	require.Equal(t, "(1 . 2)", slip.Cons{slip.Integer(2), slip.Integer(1)}.String())
	require.Equal(t, "(nil)", slip.Cons{nil}.String())
	require.Equal(t, "(nil . 1)", slip.Cons{slip.Integer(1), nil}.String())
	require.Equal(t, "(1 2 3 nil)", slip.Cons{nil, slip.Integer(3), slip.Integer(2), slip.Integer(1)}.String())
}

func TestConsCar(t *testing.T) {
	require.Equal(t, "1", slip.ObjectString(slip.Cons{slip.Integer(2), slip.Integer(1)}.Car()))
	require.Equal(t, "nil", slip.ObjectString(slip.Cons{}.Car()))
}

func TestConsCdr(t *testing.T) {
	require.Equal(t, "2", slip.ObjectString(slip.Cons{slip.Integer(2), slip.Integer(1)}.Cdr()))
	require.Equal(t, "nil", slip.ObjectString(slip.Cons{slip.Integer(1)}.Cdr()))
	require.Equal(t, "(2 3)", slip.ObjectString(slip.Cons{slip.Integer(3), slip.Integer(2), slip.Integer(1)}.Cdr()))
}
