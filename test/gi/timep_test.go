// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTimepTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(timep @2022-07-10T17:29:21.123456789Z)`,
		Expect: "t",
	}).Test(t)
}

func TestTimepFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(timep 7)`,
		Expect: "nil",
	}).Test(t)
}

func TestTimepBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(timep)`,
		Panics: true,
	}).Test(t)
}
