// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCharUpcase(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-upcase #\A)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-upcase #\a)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-upcase #\π)`,
		Expect: `#\Π`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(char-upcase #\-)`,
		Expect: `#\-`,
	}).Test(t)
}

func TestCharUpcaseNotCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-upcase 7)`,
		Panics: true,
	}).Test(t)
}
