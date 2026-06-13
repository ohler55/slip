// Copyright (c) 2023=6, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestStringAppendEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-append)`,
		Expect: `""`,
	}).Test(t)
}

func TestStringAppendMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-append "abc" 'def)`,
		Expect: `"abcdef"`,
	}).Test(t)
}

func TestStringAppendBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(string-append "x" 3)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
