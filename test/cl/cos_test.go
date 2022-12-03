// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCosFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos 0)`,
		Expect: "1",
	}).Test(t)
}

func TestCosSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos 0.0s0)`,
		Expect: "1",
	}).Test(t)
}

func TestCosDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos pi)`,
		Expect: "-1",
	}).Test(t)
}

func TestCosLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos 1.0l0)`,
		Expect: "0.5403023058681398",
	}).Test(t)
}

func TestCosRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos 1/2)`,
		Expect: "0.8775825618903728",
	}).Test(t)
}

func TestCosBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos)`,
		Panics: true,
	}).Test(t)
}

func TestCosNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(cos t)`,
		Panics: true,
	}).Test(t)
}
