// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRegexFindTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-find "a.c" "xabcx")`,
		Expect: `"abc"`,
	}).Test(t)
}

func TestRegexFindFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-find "a.c" "abbc")`,
		Expect: "nil",
	}).Test(t)
}

func TestRegexFindBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-find "a" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRegexFindBadRegex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-find t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
