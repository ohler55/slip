// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharCodeOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-code #\A)`,
		Expect: "65",
	}).Test(t)
}

func TestCharCodeNotChar(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-code 7)`,
		Panics: true,
	}).Test(t)
}
