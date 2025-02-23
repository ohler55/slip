// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGetDecodedTime(t *testing.T) {
	(&sliptest.Function{
		Source: "(nth-value 8 (get-decoded-time))",
		// Expect 9 values. The last should be a number.
		Expect: "/^-{0,1}[0-9]+$/",
	}).Test(t)
}
