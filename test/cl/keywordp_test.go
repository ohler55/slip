// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestKeywordpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(keywordp :abc)`,
		Expect: "t",
	}).Test(t)
}

func TestKeywordpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(keywordp t)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(keywordp 'abc)`,
		Expect: "nil",
	}).Test(t)
}
