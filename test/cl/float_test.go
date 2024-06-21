// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFloatBasic(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(float 3)`,
		Readably: true,
		Expect:   "3d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float 2.5s0)`,
		Readably: true,
		Expect:   "2.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float 2.5d0)`,
		Readably: true,
		Expect:   "2.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float 2.5l0)`,
		Readably: true,
		Expect:   "2.5d+00",
	}).Test(t)
}

func TestFloatPrototype(t *testing.T) {
	key := slip.Symbol("*print-readably*")
	orig, _ := slip.GetVar(key)
	slip.SetVar(key, slip.True)
	defer slip.SetVar(key, orig)

	(&sliptest.Function{
		Source:   `(float 3 0.0s0)`,
		Readably: true,
		Expect:   "3s+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float 2.5 0.0d0)`,
		Readably: true,
		Expect:   "2.5d+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float 2.5 0.0L0)`,
		Readably: true,
		Expect:   "2.5L+00",
	}).Test(t)
	(&sliptest.Function{
		Source:   `(float 2.5L0 0.0l0)`,
		Readably: true,
		Expect:   "2.5L+00",
	}).Test(t)
}

func TestFloatBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(float)`,
		Panics: true,
	}).Test(t)
}

func TestFloatNotNumber(t *testing.T) {
	(&sliptest.Function{
		Source: `(float t)`,
		Panics: true,
	}).Test(t)
}

func TestFloatBadPrototype(t *testing.T) {
	(&sliptest.Function{
		Source: `(float 3 t)`,
		Panics: true,
	}).Test(t)
}
