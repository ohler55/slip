// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestVectorEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(vector)",
		Array:  true,
		Expect: "#()",
	}).Test(t)
}

func TestVectorBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(vector 'a 'b 'c)",
		Array:  true,
		Expect: "#(a b c)",
	}).Test(t)
}
