// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestUUIDValues(t *testing.T) {
	scope := slip.NewScope()
	u := gi.UUID{0x6ef16994701d44d5, 0x87ec7ef3e2e5709b}
	scope.Let("uu", u)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(uuid-values uu)`,
		Expect: "(7994286899816383701 -8652401198136725349)",
	}).Test(t)

	(&sliptest.Function{
		Source:    `(uuid-values t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
