// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLcmNone(t *testing.T) {
	(&sliptest.Function{
		Source: `(lcm)`,
		Expect: "1",
	}).Test(t)
}

func TestLcmOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(lcm 8)`,
		Expect: "8",
	}).Test(t)
}

func TestLcmTwo(t *testing.T) {
	(&sliptest.Function{
		Source: `(lcm 42 70)`,
		Expect: "210",
	}).Test(t)
}

func TestLcmMulti(t *testing.T) {
	(&sliptest.Function{
		Source: `(lcm 42 70 8)`,
		Expect: "840",
	}).Test(t)
}

func TestLcmNeg(t *testing.T) {
	(&sliptest.Function{
		Source: `(lcm -42 70 -8)`,
		Expect: "840",
	}).Test(t)
}

func TestLcmZero(t *testing.T) {
	(&sliptest.Function{
		Source: `(lcm 8 0)`,
		Expect: "0",
	}).Test(t)
}

func TestLcmNotFixnum(t *testing.T) {
	(&sliptest.Function{
		Source:    `(lcm t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
