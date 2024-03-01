// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReplaceAllOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace-all "Like a duck." "duck" "quux")`,
		Expect: `"Like a quux."`,
	}).Test(t)
}

func TestReplaceAllRegex(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace-all "Like a duck duck." ".uck" "quux" :regex t)`,
		Expect: `"Like a quux quux."`,
	}).Test(t)
}

func TestReplaceAllBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace-all t "a" "b")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReplaceAllBadOld(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace-all "abc" t "b")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReplaceAllBadNew(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace-all "abc" "a" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
