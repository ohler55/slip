// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestOneminusFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- 3)`,
		Expect: "2",
	}).Test(t)
}

func TestOneminusSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- 3.1s0)`,
		Expect: "2.1",
	}).Test(t)
}

func TestOneminusDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- 3.1d0)`,
		Expect: "2.1",
	}).Test(t)
}

func TestOneminusLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- 3.5l0)`,
		Expect: "2.5",
	}).Test(t)
}

func TestOneminusRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- 3/4)`,
		Expect: "-1/4",
	}).Test(t)
}

func TestOneminusBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- 100000000000000000000)`,
		Expect: "99999999999999999999",
	}).Test(t)
}

func TestOneminusComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- #C(1 2))`,
		Expect: "#C(0 2)",
	}).Test(t)
}

func TestOneminusOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- (coerce 3 'octet))`,
		Expect: "2",
	}).Test(t)
}

func TestOneminusBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(1-)`,
		Panics: true,
	}).Test(t)
}

func TestOneminusNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(1- t)`,
		Panics: true,
	}).Test(t)
}
