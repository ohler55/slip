// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip/sliptest"
)

func TestSleepOk(t *testing.T) {
	start := time.Now()
	(&sliptest.Function{
		Source: `(sleep 0.001)`,
		Expect: "nil",
	}).Test(t)
	dur := time.Since(start)
	tt.Equal(t, true, time.Millisecond < dur)
}

func TestSleepBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(sleep)`,
		Panics: true,
	}).Test(t)
}

func TestSleepNotReal(t *testing.T) {
	(&sliptest.Function{
		Source: `(sleep t)`,
		Panics: true,
	}).Test(t)
}
