// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGc(t *testing.T) {
	(&sliptest.Function{
		Source: `(gc)`,
		Expect: "nil",
	}).Test(t)
}
