// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharIntOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-int #\A)`,
		Expect: "65",
	}).Test(t)
}

func TestCharIntNotChar(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-int 7)`,
		Panics: true,
	}).Test(t)
}
