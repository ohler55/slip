// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRegexMatchTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-match "^Like .* duck.$" "Like a duck.")`,
		Expect: "t",
	}).Test(t)
}

func TestRegexMatchFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-match "^Like .* duck.$" "Not like a duck.")`,
		Expect: "nil",
	}).Test(t)
}

func TestRegexMatchBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-match "a" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRegexMatchBadRegex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-match t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
