// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestParseIntegerSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer " 123 ")`,
		Expect: "123, 5",
	}).Test(t)
}

func TestParseIntegerRadix(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer " +123 " :radix 5 :start 1 :end 5)`,
		Expect: "38, 5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer "abc" :radix 16)`,
		Expect: "2748, 3",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer "zZ" :radix 36)`,
		Expect: "1295, 2",
	}).Test(t)
}

func TestParseIntegerJunkAllowed(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer " -123ᐊ" :radix 4 :junk-allowed t)`,
		Expect: "-27, 5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " -123^" :radix 4 :junk-allowed t)`,
		Expect: "-27, 5",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " x123" :junk-allowed t)`,
		Expect: "nil, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " ^123" :junk-allowed t)`,
		Expect: "nil, 1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " 123 4" :junk-allowed t)`,
		Expect: "123, 5",
	}).Test(t)
}

func TestParseIntegerJunk(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer " -123ᐊ")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " -123^")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " -123x")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " x123")`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer " ^123")`,
		Panics: true,
	}).Test(t)
}

func TestParseIntegerBig(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer "1Y2P0IJ32E8EE" :radix 36)`,
		Expect: "9223372036854775814, 13",
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer "1Y2P0IJ32E8EE00" :radix 36)`,
		Expect: "11953490159763789454944, 15",
	}).Test(t)
}

func TestParseIntegerNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer t)`,
		Panics: true,
	}).Test(t)
}

func TestParseIntegerNotKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer "123" t t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer "123" :bad t)`,
		Panics: true,
	}).Test(t)
}

func TestParseIntegerNoKeywordValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer "123" :start)`,
		Panics: true,
	}).Test(t)
}

func TestParseIntegerBadKeywordValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer "123" :start t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer "123" :end t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-integer "123" :radix t)`,
		Panics: true,
	}).Test(t)
}

func TestParseIntegerBadStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-integer "123" :start 2 :end 1)`,
		Panics: true,
	}).Test(t)
}
