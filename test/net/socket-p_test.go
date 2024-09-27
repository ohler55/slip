// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSocketPTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(socket-p (make-instance 'socket))`,
		Expect: "t",
	}).Test(t)
}

func TestSocketPFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(socket-p (make-instance 'vanilla-flavor))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(socket-p 7)`,
		Expect: "nil",
	}).Test(t)
}
