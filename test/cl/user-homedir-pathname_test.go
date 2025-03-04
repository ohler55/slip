// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestUserHomedirPathnameInherit(t *testing.T) {
	(&sliptest.Function{
		Source: "(user-homedir-pathname)",
		Expect: `/"\/.*\/*."/`,
	}).Test(t)
}
