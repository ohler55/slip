// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPrefixpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(prefixp "gi:fun" "gi:")`,
		Expect: `t`,
	}).Test(t)
}

func TestPrefixpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(prefixp "gi:fun" "cl:")`,
		Expect: `nil`,
	}).Test(t)
}

func TestPrefixpBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(prefixp t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestPrefixpBadPrefix(t *testing.T) {
	(&sliptest.Function{
		Source:    `(prefixp "abc" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
