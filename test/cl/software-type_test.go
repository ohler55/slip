// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSoftwareType(t *testing.T) {
	(&sliptest.Function{
		Source: `(software-type)`,
		Expect: `/^"SLIP.*"/`,
	}).Test(t)
}
