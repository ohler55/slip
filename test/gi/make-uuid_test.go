// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeUUIDList(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-uuid '(7994286899816383701 -8652401198136725349))`,
		Expect: "#<uuid 6ef16994-701d-44d5-87ec-7ef3e2e5709b>",
	}).Test(t)

	(&sliptest.Function{
		Source:    `(make-uuid '(t t))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-uuid '(123 t))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-uuid '(123))`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}

func TestMakeUUIDString(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-uuid "6ef16994-701d-44d5-87ec-7ef3e2e5709b")`,
		Expect: "#<uuid 6ef16994-701d-44d5-87ec-7ef3e2e5709b>",
	}).Test(t)
}

func TestMakeUUIDRandom(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-uuid)`,
		Expect: "/^#<uuid [0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89abc][0-9a-f]{3}-[0-9a-f]{12}>$/",
	}).Test(t)
}

func TestMakeUUIDBadValue(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-uuid t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
