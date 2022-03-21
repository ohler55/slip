// Copyright (c) 2022, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

func TestPrintANSI(t *testing.T) {
	key := slip.Symbol("*print-ansi*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, has = slip.GetVar(key)
	require.NotNil(t, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)
}

// TBD print-array

func TestPrintBase(t *testing.T) {
	key := slip.Symbol("*print-base*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.Fixnum(16))
	var val slip.Object
	val, has = slip.GetVar(key)
	require.NotNil(t, val)
	require.Equal(t, slip.Fixnum(16), val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.True) })

	// TBD Append and make sure fixnum is base 16

}
