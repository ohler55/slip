// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSplitOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(split "Like a duck." " ")`,
		Expect: `("Like" "a" "duck.")`,
	}).Test(t)
}

func TestSplitRegex(t *testing.T) {
	(&sliptest.Function{
		Source: `(split "Like a duck." "[ \\.]" :regex t)`,
		Expect: `("Like" "a" "duck" "")`,
	}).Test(t)
}

func TestSplitRegexOmit(t *testing.T) {
	(&sliptest.Function{
		Source: `(split "Like a duck." "[ \\.]" :regex t :omit-empty t)`,
		Expect: `("Like" "a" "duck")`,
	}).Test(t)
}

func TestSplitLimit(t *testing.T) {
	(&sliptest.Function{
		Source: `(split "abcxyzdefxyz" "xyz" :limit 2)`,
		Expect: `("abc" "defxyz")`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(split "abcxyzdefxyz" "xyz" :limit 0)`,
		Expect: `nil`,
	}).Test(t)
}

func TestSplitBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(split t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestSplitBadSeparator(t *testing.T) {
	(&sliptest.Function{
		Source:    `(split "abc" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestSplitBadLimit(t *testing.T) {
	(&sliptest.Function{
		Source:    `(split "abc" "b" :limit t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
