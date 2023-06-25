// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestIdentify(t *testing.T) {
	(&sliptest.Function{
		Source: `(identify 4)`,
		Expect: "4",
	}).Test(t)
}
