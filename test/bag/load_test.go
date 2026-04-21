// Copyright (c) 2026, Peter Ohler, All rights reserved.

package bag_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestBagLoad(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (load-bag "testdata/sample.json") :write nil :pretty t)`,
		Expect: `"{a: 1 b: 2}"`,
	}).Test(t)
}

func TestBagLoadError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(load-bag "quux.json")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}
