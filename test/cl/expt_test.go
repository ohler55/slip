// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestExptFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt 2 3)`,
		Expect: "8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(expt -2 3)`,
		Expect: "-8",
	}).Test(t)
	(&sliptest.Function{
		Source: `(expt 8 -1)`,
		Expect: "0.125",
	}).Test(t)
}

func TestExptRealReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt 1.5 2.0)`,
		Expect: "2.25",
	}).Test(t)
}

func TestExptRealComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt 2 #C(1 2))`,
		Expect: "#C(0.36691394948660344 1.9660554808224875)",
	}).Test(t)
}

func TestExptComplexReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt #C(1 2) 2)`,
		Expect: "#C(-2.999999999999999 4.000000000000001)",
		Validate: func(t *testing.T, result slip.Object) {
			c, ok := result.(slip.Complex)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, -3.1 < real(c) && real(c) < -2.9)
			tt.Equal(t, true, 3.99 < imag(c) && imag(c) < 4.01)
		},
	}).Test(t)
}

func TestExptComplexComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt #C(1 2) #C(1 1))`,
		Expect: "#C(-0.24720004426291722 0.6964504870825432)",
	}).Test(t)
}

func TestExptBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt)`,
		Panics: true,
	}).Test(t)
}

func TestExptNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(expt t 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(expt 2 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(expt #C(1 1) t)`,
		Panics: true,
	}).Test(t)
}
