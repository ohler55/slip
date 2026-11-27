// Copyright (c) 2026, Peter Ohler, All rights reserved.

package wos_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestWospTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(wosp (make-wos))`,
		Expect: "t",
	}).Test(t)
}

func TestWospFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(wosp 7)`,
		Expect: "nil",
	}).Test(t)
}
