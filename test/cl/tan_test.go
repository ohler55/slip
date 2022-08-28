// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTanFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan 0)`,
		Expect: "0",
	}).Test(t)
}

func TestTanTangleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan 0.0s0)`,
		Expect: "0",
	}).Test(t)
}

func TestTanDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan 0.0d0)`,
		Expect: "0",
	}).Test(t)
}

func TestTanLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan 0.0l0)`,
		Expect: "0",
	}).Test(t)
}

func TestTanRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan 0/2)`,
		Expect: "0",
	}).Test(t)
}

func TestTanBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan)`,
		Panics: true,
	}).Test(t)
}

func TestTanNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(tan t)`,
		Panics: true,
	}).Test(t)
}
