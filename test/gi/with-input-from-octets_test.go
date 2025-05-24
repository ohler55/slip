// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestWithInputFromOctetsBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(with-input-from-octets (s "abc def") (read-all s))`,
		Expect: `"abc def"`,
	}).Test(t)
}

func TestWithInputFromOctetsBadArgs(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-octets t (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestWithInputFromOctetsBadVar(t *testing.T) {
	(&sliptest.Function{
		Source:    `(with-input-from-octets (t "abc") (read s))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
