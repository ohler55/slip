// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRegexSubmatchTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-submatch "^Like .* ([a-z]+).$" "Like a duck.")`,
		Expect: `("Like a duck." "duck")`,
	}).Test(t)
}

func TestRegexSubmatchFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-submatch "^Like .* (D[a-z]+).$" "Not like a duck.")`,
		Expect: "nil",
	}).Test(t)
}

func TestRegexSubmatchBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-submatch "a" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRegexSubmatchBadRegex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-submatch t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
