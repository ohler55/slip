// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestStringToOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-to-octets "TEST_VAR")`,
		Expect: "(84 69 83 84 95 86 65 82)",
	}).Test(t)
}

func TestStringToOctetsBadArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(string-to-octets 2 )`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(setenv)`,
		Panics: true,
	}).Test(t)
}
