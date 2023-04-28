// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDecfSymbol(t *testing.T) {
	// verify decf returns the modified value
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x))`,
		Expect: "-1",
	}).Test(t)
	// verify x modified
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x) x)`,
		Expect: "-1",
	}).Test(t)
}

func TestDecfPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (list 1 2))) (decf (car x) 2.5) x)`,
		Expect: "(-1.5 2)",
	}).Test(t)
}

func TestDecfPlacerNested(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x (list 1 2))) (decf (car (cdr x)) 2.5) x)`,
		Expect: "(1 -0.5)",
	}).Test(t)
}

func TestDecfSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x 1.5s+0))`,
		Expect: "-1.5",
	}).Test(t)
}

func TestDecfLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x 1.5L+0))`,
		Expect: "-1.5",
	}).Test(t)
}

func TestDecfBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x 100000000000000000000))`,
		Expect: "-100000000000000000000",
	}).Test(t)
}

func TestDecfRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x 3/4))`,
		Expect: "-3/4",
	}).Test(t)
}

func TestDecfComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x #C(1 2)))`,
		Expect: "#C(-1 -2)",
	}).Test(t)
}

func TestDecfBadPlacer(t *testing.T) {
	(&sliptest.Function{
		Source: `(decf t 1)`,
		Panics: true,
	}).Test(t)
}

func TestDecfBadDelta(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x 0)) (decf x t))`,
		Panics: true,
	}).Test(t)
}
