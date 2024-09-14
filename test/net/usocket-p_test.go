// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestUsocketPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(usocket-p (make-instance 'usocket))`,
		Expect: "t",
	}).Test(t)
}

func TestUsocketPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(usocket-p (make-instance 'vanilla-flavor))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(usocket-p 7)`,
		Expect: "nil",
	}).Test(t)
}
