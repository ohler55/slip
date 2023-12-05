// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestOctetsToString(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string '(84 69 83 84 95 86 65 82))`,
		Expect: `"TEST_VAR"`,
	}).Test(t)
}

func TestOctetsToStringEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string '())`,
		Expect: `""`,
	}).Test(t)
}

func TestOctetsToStringBadArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string 4)`,
		Panics: true,
	}).Test(t)
}

func TestOctetsToStringBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string '("foo" "bar"))`,
		Panics: true,
	}).Test(t)
}
