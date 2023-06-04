// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGetInternalRunTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(get-internal-run-time)`,
		Expect: "/^[0-9]+$/",
	}).Test(t)
}
