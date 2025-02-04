// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestLambdaListKeywordsBuiltIn(t *testing.T) {
	(&sliptest.Function{
		Source: `(lambda-list-keywords)`,
		Expect: "(&allow-other-keys &aux &body &key &optional &rest)",
	}).Test(t)
}
