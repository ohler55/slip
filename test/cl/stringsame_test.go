// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringSameTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string= "abc" "abc")`,
		Expect: "t",
	}).Test(t)
}

func TestStringSameFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string= "abc" "Abc")`,
		Expect: "nil",
	}).Test(t)
}
