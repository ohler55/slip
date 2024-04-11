// Copyright (c) 2024, Peter Ohler, All rights reserved.

package watch_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPkg(t *testing.T) {
	(&sliptest.Function{
		Source: `*watch*`,
		Expect: "#<package watch>",
	}).Test(t)
}
