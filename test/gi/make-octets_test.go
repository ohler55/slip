// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeOctetsZero(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-octets 3)`,
		Array:  true,
		Expect: "#(0 0 0)",
	}).Test(t)
}

func TestMakeOctetsInitialElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-octets 3 #\a)`,
		Array:  true,
		Expect: "#(97 97 97)",
	}).Test(t)
}

func TestMakeOctetsBadValue(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-octets t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
