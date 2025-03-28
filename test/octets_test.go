// Copyright (c) 2025, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOctets(t *testing.T) {
	(&sliptest.Object{
		Target:    slip.Octets("abc"),
		String:    `#(97 98 99)`,
		Simple:    []any{97, 98, 99},
		Hierarchy: "octets.vector.array.sequence.t",
		Equals: []*sliptest.EqTest{
			{Other: slip.Octets("abc"), Expect: true},
			{Other: slip.Octets("ABC"), Expect: false},
			{Other: slip.True, Expect: false},
		},
		Eval: slip.Octets("abc"),
	}).Test(t)
	tt.Equal(t, slip.OctetSymbol, slip.Octets("x").SequenceType())
	tt.Equal(t, slip.OctetsSymbol, slip.Octets("x").ArrayType())
	tt.Equal(t, 3, slip.Octets("abc").Length())
	tt.Equal(t, slip.Octets("aaa"), slip.NewOctets(3, slip.Octet(97)))
	tt.Equal(t, slip.List{slip.Octet(97), slip.Octet(97), slip.Octet(97)},
		slip.NewOctets(3, slip.Octet(97)).AsList())
	tt.Equal(t, false, slip.Octets("abc").Adjustable())
	tt.Equal(t, slip.OctetSymbol, slip.Octets("abc").ElementType())
	tt.Equal(t, 1, slip.Octets("abc").Rank())
	tt.Equal(t, []int{3}, slip.Octets("abc").Dimensions())
}

func TestOctetsAdjustInitialElement(t *testing.T) {
	octets := slip.Octets("abc")

	dup := octets.Adjust([]int{5}, slip.OctetSymbol, slip.Fixnum(98), nil, -1)
	tt.Equal(t, "#(97 98 99)", slip.ObjectString(octets))
	tt.Equal(t, "#(97 98 99 98 98)", slip.ObjectString(dup))
	tt.NotEqual(t, octets, dup)

	tt.Panic(t, func() { _ = octets.Adjust([]int{5, 6}, slip.OctetSymbol, slip.Fixnum(98), nil, -1) })
	tt.Panic(t, func() { _ = octets.Adjust([]int{5}, slip.FixnumSymbol, slip.Fixnum(98), nil, -1) })
	tt.Panic(t, func() { _ = octets.Adjust([]int{5}, slip.OctetSymbol, slip.Fixnum(300), nil, -1) })
}

func TestOctetsAdjustInitialContents(t *testing.T) {
	octets := slip.Octets("abc")

	dup := octets.Adjust([]int{4}, slip.OctetSymbol, slip.Fixnum(98),
		slip.List{slip.Octet(97), slip.Octet(98), slip.Octet(99), slip.Octet(100)}, -1)
	tt.Equal(t, "#(97 98 99)", slip.ObjectString(octets))
	tt.Equal(t, "#(97 98 99 100)", slip.ObjectString(dup))
	tt.NotEqual(t, octets, dup)

	tt.Panic(t, func() { _ = octets.Adjust([]int{5}, slip.OctetSymbol, slip.Octet(0), slip.List{slip.Octet(99)}, -1) })
	tt.Panic(t, func() { _ = octets.Adjust([]int{1}, slip.OctetSymbol, slip.Octet(0), slip.List{slip.Fixnum(-1)}, -1) })
}

func TestOctetsGet(t *testing.T) {
	octets := slip.Octets("abc")
	val := octets.Get(1)
	tt.Equal(t, slip.Octet(98), val)
	tt.Panic(t, func() { _ = octets.Get(1, 2) })
	tt.Panic(t, func() { _ = octets.Get(5) })
}

func TestOctetsSet(t *testing.T) {
	octets := slip.Octets("abc")
	octets.Set(slip.Fixnum(100), 1)
	val := octets.Get(1)
	tt.Equal(t, slip.Octet(100), val)
	tt.Panic(t, func() { octets.Set(slip.Octet(1), 1, 2) })
	tt.Panic(t, func() { octets.Set(slip.Octet(1), 5) })
	tt.Panic(t, func() { octets.Set(slip.Fixnum(300), 1) })
}

func TestOctetsMajorIndex(t *testing.T) {
	octets := slip.Octets("abc")
	tt.Equal(t, 1, octets.MajorIndex(1))

	octets.Set(slip.Fixnum(100), 1)
	val := octets.Get(1)
	tt.Equal(t, slip.Octet(100), val)
	tt.Panic(t, func() { _ = octets.MajorIndex(3) })
	tt.Panic(t, func() { _ = octets.MajorIndex(1, 2) })
}

func TestOctetsMajorSet(t *testing.T) {
	octets := slip.Octets("abc")
	octets.MajorSet(1, slip.Octet('x'))
	tt.Equal(t, "#(97 120 99)", octets.String())

	tt.Panic(t, func() { octets.MajorSet(3, slip.Octet(1)) })
}
