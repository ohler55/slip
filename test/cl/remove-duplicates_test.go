// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRemoveDuplicatesListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(remove-duplicates '(1 2 3 2 1))",
		Expect: "(3 2 1)",
	}).Test(t)
}
