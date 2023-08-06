// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMachineType(t *testing.T) {
	(&sliptest.Function{
		Source: `(machine-type)`,
		Expect: `/.*/`,
	}).Test(t)
}
