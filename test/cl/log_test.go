// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLogRealReal2(t *testing.T) {
	(&sliptest.Function{
		Source: `(log 128 2)`,
		Expect: "7",
	}).Test(t)
}

func TestLogRealReal10(t *testing.T) {
	(&sliptest.Function{
		Source: `(log 100 10)`,
		Expect: "2",
	}).Test(t)
}

func TestLogRealRealE(t *testing.T) {
	(&sliptest.Function{
		Source: `(log 100)`,
		Expect: "4.605170185988092",
	}).Test(t)
}

func TestLogRealRealN(t *testing.T) {
	(&sliptest.Function{
		Source: `(log 25 5)`,
		Expect: "2",
	}).Test(t)
}

func TestLogRealComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(log 2 #C(1 1))`,
		Expect: "#C(0.32596797216345985 -0.7387021222729507)",
	}).Test(t)
}

func TestLogComplexReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(log #C(1 1) 2)`,
		Expect: "#C(0.5000000000000001 1.1330900354567985)",
	}).Test(t)
}

func TestLogComplexComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(log #C(2 3) #C(1 1))`,
		Validate: func(t *testing.T, result slip.Object) {
			c, ok := result.(slip.Complex)
			tt.Equal(t, true, ok)
			tt.Equal(t, true, 1.65 < real(c) && real(c) < 1.66)
			tt.Equal(t, true, -1.0 < imag(c) && imag(c) < -0.9)
		},
	}).Test(t)
}

func TestLogBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(log)`,
		Panics: true,
	}).Test(t)
}

func TestLogNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(log t 2)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(log 2 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(log #C(1 1) t)`,
		Panics: true,
	}).Test(t)
}
