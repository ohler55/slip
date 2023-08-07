// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMachineVersion(t *testing.T) {
	(&sliptest.Function{
		Source: `(machine-version)`,
		Expect: `/.*/`,
	}).Test(t)
}
