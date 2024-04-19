// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogandc1Fixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(logandc1 3 7)`,
		Expect: "4",
	}).Test(t)
}

func TestLogandc1Bignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc1 #x33333333333333333333 #x787878787878787878))`,
		Expect: `"484848484848484848"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc1 #x333333333333333333 #x78787878787878787878))`,
		Expect: `"78484848484848484848"`,
	}).Test(t)
}

func TestLogandc1Mixed(t *testing.T) {
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc1 #x333333333333333333 #x7878787878787878))`,
		Expect: `"4848484848484848"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(format nil "~X" (logandc1 #x3333333333333333 #x787878787878787878))`,
		Expect: `"784848484848484848"`,
	}).Test(t)
}

func TestLogandc1NotInteger(t *testing.T) {
	(&sliptest.Function{
		Source:    `(logandc1 t 1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logandc1 1 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(logandc1 #x1234567890abcdef0 t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
