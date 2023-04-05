// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNintersectionListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nintersection '(a b c b) '(d b e))`,
		Expect: "(b)",
	}).Test(t)
}
