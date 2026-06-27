// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAppArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `*app-args*`,
		Expect: `/gi.test/`,
	}).Test(t)
}
