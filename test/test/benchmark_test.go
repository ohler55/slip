// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBenchmark(t *testing.T) {
	(&sliptest.Function{
		Source: `(benchmark (lambda () (sleep 0.01)) 0.1)`,
		Expect: "/^0.01[0-9]+$/",
	}).Test(t)
}

func TestBenchmarkIter(t *testing.T) {
	(&sliptest.Function{
		Source: `(benchmark (lambda () (sleep 0.01)) 0.5 10)`,
		Expect: "/^0.01[0-9]+$/",
	}).Test(t)
}

func TestBenchmarkBadDuration(t *testing.T) {
	(&sliptest.Function{
		Source: `(benchmark (lambda () (sleep 0.01)) t)`,
		Panics: true,
	}).Test(t)
}

func TestBenchmarkBadIterations(t *testing.T) {
	(&sliptest.Function{
		Source: `(benchmark (lambda () (sleep 0.01)) 0.1 t)`,
		Panics: true,
	}).Test(t)
}
