// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTrimSuffixOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(trim-suffix "my-file.lisp" ".lisp")`,
		Expect: `"my-file"`,
	}).Test(t)
}

func TestTrimSuffixBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(trim-suffix t "a")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestTrimSuffixBadSuffix(t *testing.T) {
	(&sliptest.Function{
		Source:    `(trim-suffix "abc" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
