// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestUUIDString(t *testing.T) {
	scope := slip.NewScope()
	u := gi.UUID{0x6ef16994701d44d5, 0x87ec7ef3e2e5709b}
	scope.Let("uu", u)
	(&sliptest.Function{
		Scope:  scope,
		Source: `(uuid-string uu)`,
		Expect: `"6ef16994-701d-44d5-87ec-7ef3e2e5709b"`,
	}).Test(t)

	(&sliptest.Function{
		Source:    `(uuid-string t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
