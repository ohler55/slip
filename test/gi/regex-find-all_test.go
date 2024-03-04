// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRegexFindAllNoLimit(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-find-all "a.c" "xabcxazc")`,
		Expect: `("abc" "azc")`,
	}).Test(t)
}

func TestRegexFindAllEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-find-all "a.c" "abbc")`,
		Expect: "nil",
	}).Test(t)
}

func TestRegexFindAllLimit(t *testing.T) {
	(&sliptest.Function{
		Source: `(regex-find-all "a.c" "xabcxazc" :limit 1)`,
		Expect: `("abc")`,
	}).Test(t)
}

func TestRegexFindAllBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-find-all "a" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRegexFindAllBadRegex(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-find-all t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestRegexFindAllBadLimit(t *testing.T) {
	(&sliptest.Function{
		Source:    `(regex-find-all "a" "b" :limit t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
