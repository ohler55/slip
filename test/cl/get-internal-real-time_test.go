// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGetInternalRealTime(t *testing.T) {
	(&sliptest.Function{
		Source: `(get-internal-real-time)`,
		Expect: "/^[0-9]+$/",
	}).Test(t)
}
