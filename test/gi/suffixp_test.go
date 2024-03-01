// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSuffixpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(suffixp "my-file.lisp" ".lisp")`,
		Expect: `t`,
	}).Test(t)
}

func TestSuffixpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(suffixp "my-file.lisp" ".txt")`,
		Expect: `nil`,
	}).Test(t)
}

func TestSuffixpBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(suffixp t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestSuffixpBadSuffix(t *testing.T) {
	(&sliptest.Function{
		Source:    `(suffixp "abc" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
