// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPrin1ToStringBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(prin1-to-string #\A)`,
		Expect: `"#\A"`,
	}).Test(t)
}
