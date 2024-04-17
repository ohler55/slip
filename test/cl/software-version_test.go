// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestSoftwareVersion(t *testing.T) {
	(&sliptest.Function{
		Source: `(software-version)`,
		Expect: `/^".*"/`,
	}).Test(t)
}
