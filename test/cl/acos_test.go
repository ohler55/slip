// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAcosFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos -1)`,
		Expect: "3.141592653589793",
	}).Test(t)
}

func TestAcosSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos 0.5s0)`,
		Expect: "1.0471975511965976",
	}).Test(t)
}

func TestAcosDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos 0.5d0)`,
		Expect: "1.0471975511965976",
	}).Test(t)
}

func TestAcosLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos 0.5l0)`,
		Expect: "1.0471975511965976",
	}).Test(t)
}

func TestAcosRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos 1/2)`,
		Expect: "1.0471975511965976",
	}).Test(t)
}

func TestAcosBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos)`,
		Panics: true,
	}).Test(t)
}

func TestAcosNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(acos t)`,
		Panics: true,
	}).Test(t)
}
