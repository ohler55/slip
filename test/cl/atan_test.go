// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAtanFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan -1)`,
		Expect: "-0.7853981633974483",
	}).Test(t)
}

func TestAtanSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan 1.0s0)`,
		Expect: "0.7853981633974483",
	}).Test(t)
}

func TestAtanDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan 1.0d0)`,
		Expect: "0.7853981633974483",
	}).Test(t)
}

func TestAtanLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan 1.0l0)`,
		Expect: "0.7853981633974483",
	}).Test(t)
}

func TestAtanRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan 3/2)`,
		Expect: "0.982793723247329",
	}).Test(t)
}

func TestAtanBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan)`,
		Panics: true,
	}).Test(t)
}

func TestAtanNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(atan t)`,
		Panics: true,
	}).Test(t)
}
