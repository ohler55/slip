// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringNeTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(string/= "abc" "Abc")`,
		Expect: "t",
	}).Test(t)
}

func TestStringNeFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(string/= "abc" "abc")`,
		Expect: "nil",
	}).Test(t)
}
