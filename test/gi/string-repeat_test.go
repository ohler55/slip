// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStringRepeatOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-repeat "Abc" 3)`,
		Expect: `"AbcAbcAbc"`,
	}).Test(t)
}

func TestStringRepeatBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(string-repeat t 3)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestStringRepeatBadCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(string-repeat "x" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
