// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"
	"time"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRunOk(t *testing.T) {
	var scope slip.Scope
	x := slip.Symbol("x")
	scope.Let(x, slip.Fixnum(7))
	(&sliptest.Function{
		Scope:  &scope,
		Source: `(run (setq x 3))`,
		Expect: "",
	}).Test(t)
	start := time.Now()
	for time.Since(start) < time.Second {
		if slip.Fixnum(3).Equal(scope.Get(x)) {
			break
		}
		time.Sleep(time.Millisecond * 10)
	}
	tt.Equal(t, slip.Fixnum(3), scope.Get(x))
}

func TestRunBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(run)`,
		Panics: true,
	}).Test(t)
}
