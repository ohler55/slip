// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestClassOfFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-of (make-instance vanilla-flavor))`,
		Expect: "#<flavor vanilla-flavor>",
	}).Test(t)
}

func TestClassOfNotInstance(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-of t)`,
		Expect: "nil",
	}).Test(t)
}

func TestClassOfArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(class-of)`,
		Panics: true,
	}).Test(t)
}
