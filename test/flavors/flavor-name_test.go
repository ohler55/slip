// Copyright (c) 2023, Peter Ohler, All rights reserved.

package flavors_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFlavorNameFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(flavor-name vanilla-flavor)`,
		Expect: "vanilla-flavor",
	}).Test(t)
	(&sliptest.Function{
		Source: `(flavor-name 'vanilla-flavor)`,
		Expect: "vanilla-flavor",
	}).Test(t)
}

func TestFlavorNameArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(flavor-name)`,
		Panics: true,
	}).Test(t)
}

func TestFlavorNameNotFlavor(t *testing.T) {
	(&sliptest.Function{
		Source: `(flavor-name t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(flavor-name 'not-a-flavor)`,
		Panics: true,
	}).Test(t)
}
