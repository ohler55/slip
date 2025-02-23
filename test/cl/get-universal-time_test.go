// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGetUniversalTime(t *testing.T) {
	(&sliptest.Function{
		Source: "(get-universal-time)",
		Expect: "/^[0-9]+$/",
	}).Test(t)
}
