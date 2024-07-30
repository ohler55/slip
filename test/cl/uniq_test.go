// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestUniqFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= 3)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestUniqSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= 3.0s+0 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3.0s+0 3)`,
		Expect: "nil",
	}).Test(t)
}

func TestUniqDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= 3.0d+0 4)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3.5s+0 4.5d+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3.5s+0 3.5d+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestUniqLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= 4 3.0L+0)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3.5s+0 3.5L+0)`,
		Expect: "nil",
	}).Test(t)
}

func TestUniqBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= 30000000000000000000 20000000000000000000)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= (- 30000000000000000100 30000000000000000000) 100)`,
		Expect: "nil",
	}).Test(t)
}

func TestUniqRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= 3/4 6/8)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3/4 0.75)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 3/4 1/2)`,
		Expect: "t",
	}).Test(t)
}

func TestUniqComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= #C(1 2) #C(1 2))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= #C(1 0) 1)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= #C(1 2) 1)`,
		Expect: "t",
	}).Test(t)
}

func TestUniqOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= (coerce 3 'octet))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= (coerce 3 'octet) (coerce 4 'octet))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= (coerce 3 'octet) (coerce 3 'octet))`,
		Expect: "nil",
	}).Test(t)
}

func TestUniqNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(/= t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(/= 1 t)`,
		Panics: true,
	}).Test(t)
}

func TestUniqNoArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(/=)`,
		Panics: true,
	}).Test(t)
}
