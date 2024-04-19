// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogandc2Fixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logandc2 7 3)`,
		Expect: "4",
	}).Test(t)
}

func TestLogandc2Bignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc2 #x787878787878787878 #x33333333333333333333))`,
		Expect: `"484848484848484848"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc2 #x78787878787878787878 #x333333333333333333))`,
		Expect: `"78484848484848484848"`,
	}).Test(t)
}

func TestLogandc2Mixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc2 #x7878787878787878 #x333333333333333333))`,
		Expect: `"4848484848484848"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc2 #x787878787878787878 #x3333333333333333))`,
		Expect: `"784848484848484848"`,
	}).Test(t)
}

func TestLogandc2NotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logandc2 t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logandc2 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logandc2 #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
