// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTrimPrefixOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(trim-prefix "gi:fun" "gi:")`,
		Expect: `"fun"`,
	}).Test(t)
}

func TestTrimPrefixBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(trim-prefix t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestTrimPrefixBadPrefix(t *testing.T) {
	(&sliptest.Function{
		Source:    `(trim-prefix "abc" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
