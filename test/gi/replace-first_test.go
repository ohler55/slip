// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestReplaceFirstOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace-first "Like a duck." "duck" "quux")`,
		Expect: `"Like a quux."`,
	}).Test(t)
}

func TestReplaceFirstRegex(t *testing.T) {
	(&sliptest.Function{
		Source: `(replace-first "Like a duck duck." ".uck" "quux" :regex t)`,
		Expect: `"Like a quux duck."`,
	}).Test(t)
}

func TestReplaceFirstBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace-first t "a" "b")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReplaceFirstBadOld(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace-first "abc" t "b")`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestReplaceFirstBadNew(t *testing.T) {
	(&sliptest.Function{
		Source:    `(replace-first "abc" "a" t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
