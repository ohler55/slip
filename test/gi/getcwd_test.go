// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGetcwd(t *testing.T) {
	(&sliptest.Function{
		Source: `(getcwd)`,
		Expect: `/slip\/test\/gi"$/`,
	}).Test(t)
}
