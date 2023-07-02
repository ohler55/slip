// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestReadFromStringDefaults(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " 123 ")`,
		Expect: `123, 5`,
	}).Test(t)
}

func TestReadFromStringPreserve(t *testing.T) {
	(&sliptest.Function{
		Source: `(read-from-string " 123 " :preserve-whitespace t)`,
		Expect: `123, 4`,
	}).Test(t)
}
