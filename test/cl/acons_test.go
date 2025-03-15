// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAconsNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(acons 'a 1 nil)",
		Expect: "((a . 1))",
	}).Test(t)
}

func TestAconsList(t *testing.T) {
	(&sliptest.Function{
		Source: "(acons 'a 1 '())",
		Expect: "((a . 1))",
	}).Test(t)
	(&sliptest.Function{
		Source: "(acons 'a 1 '((b . 2)))",
		Expect: "((a . 1) (b . 2))",
	}).Test(t)
}

func TestAconsObject(t *testing.T) {
	(&sliptest.Function{
		Source: "(acons 'a 1 2)",
		Expect: "((a . 1) . 2)",
	}).Test(t)
}
