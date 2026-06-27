// Copyright (c) 2026, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestParseURLOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (parse-url "http://you:secret@localhost:12345/home?quux=abc") :scheme)`,
		Expect: `"http"`,
	}).Test(t)
}

func TestParseURLError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-url "http://localhost:xyz")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
