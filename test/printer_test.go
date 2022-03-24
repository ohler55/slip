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
	val, _ = slip.GetVar(key)
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
	val, _ = slip.GetVar(key)
	require.NotNil(t, val)
	require.Equal(t, slip.Fixnum(16), val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	require.Panics(t, func() { slip.SetVar(key, slip.True) })

	radixKey := slip.Symbol("*print-radix*")
	origRadix, _ := slip.GetVar(radixKey)
	defer slip.SetVar(radixKey, origRadix)
	slip.SetVar(radixKey, slip.True)

	obj := slip.Fixnum(37)
	for _, pair := range []*struct {
		base   int
		expect string
	}{
		{base: 2, expect: "#b100101"},
		{base: 3, expect: "#3r1101"},
		{base: 8, expect: "#o45"},
		{base: 10, expect: "37."},
		{base: 16, expect: "#x25"},
	} {
		slip.SetVar(key, slip.Fixnum(pair.base))
		require.Equal(t, pair.expect, string(slip.Append([]byte{}, obj)), "%d: printer append", pair.base)
		require.Equal(t, pair.expect, obj.String(), "base %d obj.Append()", pair.base)
	}
}

// TBD Case Symbol
// TBD Circle bool
// TBD Escape bool
// TBD Gensym bool
// TBD Length uint
// TBD Level uint
// TBD Lines uint
// TBD MiserWidth uint

func TestPrintPretty(t *testing.T) {
	key := slip.Symbol("*print-pretty*")
	orig, has := slip.GetVar(key)
	require.True(t, has)
	defer slip.SetVar(key, orig)

	slip.SetVar(key, slip.True)
	var val slip.Object
	val, _ = slip.GetVar(key)
	require.NotNil(t, val)
	require.Equal(t, slip.True, val)

	doc := slip.DescribeVar(key)
	require.NotEqual(t, "", doc)

	widthKey := slip.Symbol("*print-right-margin*")
	origWidth, _ := slip.GetVar(widthKey)
	defer slip.SetVar(widthKey, origWidth)

	slip.SetVar(widthKey, slip.Fixnum(20))

	obj := makeTestTree(3)
	out := slip.Append([]byte{}, obj)
	expect := `(0 1 (0) 2 (0 1 (0))
   3
   (0 1 (0) 2
      (0 1 (0))))
`
	require.Equal(t, expect, string(out))
	// flat
	slip.SetVar(key, nil)
	expect = `(0 1 (0) 2 (0 1 (0)) 3 (0 1 (0) 2 (0 1 (0))))`
	out = slip.Append([]byte{}, obj)
	require.Equal(t, expect, string(out))
}

// TBD Radix bool
// TBD Readably bool
// TBD RightMargin uint

func makeTestTree(n int) slip.Object {
	var list slip.List
	for ; 0 <= n; n-- {
		if 0 < n {
			list = append(list, makeTestTree(n-1))
		}
		list = append(list, slip.Fixnum(n))
	}
	return list
}
