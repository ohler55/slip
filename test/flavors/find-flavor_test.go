// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFindFlavorFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-flavor 'vanilla-flavor)`,
		Expect: "#<flavor vanilla-flavor>",
	}).Test(t)
}

func TestFindFlavorNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-flavor 'not-a-flavor)`,
		Expect: "nil",
	}).Test(t)
}

func TestFindFlavorNotFoundPanic(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-flavor 'not-a-flavor t)`,
		Panics: true,
	}).Test(t)
}

func TestFindFlavorArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-flavor)`,
		Panics: true,
	}).Test(t)
}

func TestFindFlavorNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-flavor t)`,
		Panics: true,
	}).Test(t)
}
