// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMachineInstance(t *testing.T) {
	(&sliptest.Function{
		Source: `(machine-instance)`,
		Expect: `/.*/`,
	}).Test(t)
}
