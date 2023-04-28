// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFlavorOfFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(flavor-of (make-instance vanilla-flavor))`,
		Expect: "#<flavor vanilla-flavor>",
	}).Test(t)
}

func TestFlavorOfNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source: `(flavor-of t)`,
		Expect: "nil",
	}).Test(t)
}

func TestFlavorOfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(flavor-of)`,
		Panics: true,
	}).Test(t)
}
