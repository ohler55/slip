// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestChannelCloseOk(t *testing.T) {
	scope := slip.NewScope()
	ch := make(chan slip.Object, 3)
	scope.Let(slip.Symbol("queue"), gi.Channel(ch))
	(&sliptest.Function{
		Scope:  scope,
		Source: `(channel-close queue)`,
		Expect: "",
	}).Test(t)
}

func TestChannelCloseBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(channel-close)`,
		Panics: true,
	}).Test(t)
}

func TestChannelCloseNotChannel(t *testing.T) {
	(&sliptest.Function{
		Source: `(channel-close t)`,
		Panics: true,
	}).Test(t)
}
