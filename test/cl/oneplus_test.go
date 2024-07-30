// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestOneplusFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ 3)`,
		Expect: "4",
	}).Test(t)
}

func TestOneplusSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ 3.1s0)`,
		Expect: "4.1",
	}).Test(t)
}

func TestOneplusDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ 3.1d0)`,
		Expect: "4.1",
	}).Test(t)
}

func TestOneplusLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ 3.5l0)`,
		Expect: "4.5",
	}).Test(t)
}

func TestOneplusRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ 3/4)`,
		Expect: "7/4",
	}).Test(t)
}

func TestOneplusBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ 100000000000000000000)`,
		Expect: "100000000000000000001",
	}).Test(t)
}

func TestOneplusComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ #C(1 2))`,
		Expect: "#C(2 2)",
	}).Test(t)
}

func TestOneplusOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ (coerce 3 'octet))`,
		Expect: "4",
	}).Test(t)
}

func TestOneplusBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+)`,
		Panics: true,
	}).Test(t)
}

func TestOneplusNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(1+ t)`,
		Panics: true,
	}).Test(t)
}
