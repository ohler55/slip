// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStringToOctetsEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-to-octets nil)`,
		Array:  true,
		Expect: "#()",
	}).Test(t)
}

func TestStringToOctetsString(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-to-octets "TEST_ぴ_" :end 6)`,
		Array:  true,
		Expect: "#(84 69 83 84 95 227 129 180)",
	}).Test(t)
}

func TestStringToOctetsList(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-to-octets '(#\a #\Space #\T #\e #\s #\t #\s) :start 2 :end 6)`,
		Array:  true,
		Expect: "#(84 101 115 116)",
	}).Test(t)
}

func TestStringToOctetsVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-to-octets (vector #\a #\Space #\T #\e #\s #\t #\s) :start 2 :end 6)`,
		Array:  true,
		Expect: "#(84 101 115 116)",
	}).Test(t)
}

func TestStringToOctetsBadArg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(string-to-octets 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestStringToOctetsBadList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(string-to-octets '(#\a t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestStringToOctetsBadStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(string-to-octets "TEST_ぴ_" :end 8)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(string-to-octets "TEST_ぴ_" :start 8)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(string-to-octets "TEST_ぴ_" :start 4 :end 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(string-to-octets "TEST_ぴ_" :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(string-to-octets "TEST_ぴ_" :end  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
