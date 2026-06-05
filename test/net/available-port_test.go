// Copyright (c) 2026, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAvailablePort(t *testing.T) {
	(&sliptest.Function{
		Source: `(available-port)`,
		Expect: "/^[0-9]+$/",
	}).Test(t)
}
