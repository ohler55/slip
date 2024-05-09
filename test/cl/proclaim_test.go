// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestProclaim(t *testing.T) {
	(&sliptest.Function{
		Source: `(proclaim (ignore 'abc))`,
		Expect: "",
	}).Test(t)
}
