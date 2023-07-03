// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestParseFloatNoExp(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.23")`,
		Expect:   "1.23d+00",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "123")`,
		Expect:   "1.23d+02",
	}).Test(t)
}

func TestParseFloatE(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.23e1")`,
		Expect:   "1.23d+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.23E1")`,
		Expect:   "1.23d+01",
	}).Test(t)
}

func TestParseFloatS(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25s1")`,
		Expect:   "1.25s+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25S1")`,
		Expect:   "1.25s+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25e1" :type 'single-float)`,
		Expect:   "1.25s+01",
	}).Test(t)
}

func TestParseFloatF(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25f1")`,
		Expect:   "1.25s+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25F1")`,
		Expect:   "1.25s+01",
	}).Test(t)
}

func TestParseFloatD(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25d1")`,
		Expect:   "1.25d+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25D1")`,
		Expect:   "1.25d+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25s1" :type 'double-float)`,
		Expect:   "1.25d+01",
	}).Test(t)
}

func TestParseFloatL(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25l1")`,
		Expect:   "1.25L+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25L1")`,
		Expect:   "1.25L+01",
	}).Test(t)
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "1.25e1" :type 'long-float)`,
		Expect:   "1.25L+01",
	}).Test(t)
}

func TestParseFloatStartEnd(t *testing.T) {
	(&sliptest.Function{
		Readably: true,
		Source:   `(parse-float "x 1.23e1 y" :start 1 :end 9)`,
		Expect:   "1.23d+01",
	}).Test(t)
}

func TestParseFloatNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-float t)`,
		Panics: true,
	}).Test(t)
}

func TestParseFloatBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-float "1.23" :bad t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(parse-float "1.23" t)`,
		Panics: true,
	}).Test(t)
}

func TestParseFloatBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-float "1.23" :start t)`,
		Panics: true,
	}).Test(t)
}

func TestParseFloatBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-float "1.23" :end t)`,
		Panics: true,
	}).Test(t)
}

func TestParseFloatBadBounds(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-float "1.23" :start 3 :end 2)`,
		Panics: true,
	}).Test(t)
}

func TestParseFloatNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-float "1.2.3")`,
		Panics: true,
	}).Test(t)
}
