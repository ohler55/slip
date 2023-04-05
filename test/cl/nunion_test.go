// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNunionListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nunion '(a b c b) '(d b e))`,
		Expect: "(a b c d e)",
	}).Test(t)
}
