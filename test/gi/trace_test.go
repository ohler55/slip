// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTraceToggle(t *testing.T) {
	(&sliptest.Function{
		Source: `(trace t)(trace nil)`,
		Expect: "",
	}).Test(t)
}

func TestTraceBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source: `(trace)`,
		Panics: true,
	}).Test(t)
}
