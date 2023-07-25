// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestRemoveIfListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(remove-if 'evenp '(1 2 3 4 5))",
		Expect: "(1 3 5)",
	}).Test(t)
}
