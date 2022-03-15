// Copyright (c) 2022, Peter Ohler, All rights reserved.

package sliptest

import (
	"strings"
	"testing"

	"github.com/ohler55/ojg/alt"
	"github.com/ohler55/slip"
	"github.com/stretchr/testify/require"
)

type EqTest struct {
	Other  slip.Object
	Expect bool
}

func TestObject(
	t *testing.T,
	obj slip.Object,
	str string,
	simple interface{},
	hierarchy string,
	eqs []*EqTest,
	selfies ...func() slip.Symbol) {

	require.Equal(t, str, obj.String())
	require.Equal(t, str, string(obj.Append([]byte{})))
	simp := obj.Simplify()
	diff := alt.Compare(simple, simp)
	require.Nil(t, diff, "difference at %v for %s", diff, obj)
	var hb []byte
	for i, sym := range obj.Hierarchy() {
		if 0 < i {
			hb = append(hb, '.')
		}
		hb = append(hb, strings.ToLower(string(sym))...)
	}
	require.Equal(t, hierarchy, string(hb))
	for _, et := range eqs {
		if et.Expect {
			require.True(t, obj.Equal(et.Other))
		} else {
			require.False(t, obj.Equal(et.Other))
		}
	}
	self := obj.Hierarchy()[0]
	for _, f := range selfies {
		require.Equal(t, self, f())
	}
}
