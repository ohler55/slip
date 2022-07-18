// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestChannelPopOk(t *testing.T) {
	var scope slip.Scope
	ch := make(chan slip.Object, 3)
	ch <- slip.Fixnum(7)
	scope.Let(slip.Symbol("queue"), gi.Channel(ch))
	defer func() { close(ch) }()
	(&sliptest.Function{
		Scope:  &scope,
		Source: `(channel-pop queue)`,
		Expect: "7",
	}).Test(t)
}

func TestChannelPopBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(channel-pop)`,
		Panics: true,
	}).Test(t)
}

func TestChannelPopNotChannel(t *testing.T) {
	(&sliptest.Function{
		Source: `(channel-pop t)`,
		Panics: true,
	}).Test(t)
}
