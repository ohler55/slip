// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

// Tested in delete_test since remove is effectively an alias for delete in
// this implementation.

func TestRemoveListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(remove 'b '(a b c b d))",
		Expect: "(a c d)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(remove 'x '(a b c))",
		Expect: "(a b c)",
	}).Test(t)
}
