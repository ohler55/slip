// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharDowncase(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-downcase #\A)`,
		Expect: `#\a`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-downcase #\a)`,
		Expect: `#\a`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-downcase #\Π)`,
		Expect: `#\π`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-downcase #\-)`,
		Expect: `#\-`,
	}).Test(t)
}

func TestCharDowncaseNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-downcase 7)`,
		Panics: true,
	}).Test(t)
}
