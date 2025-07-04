// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestChannelPushOk(t *testing.T) {
	scope := slip.NewScope()
	ch := make(chan slip.Object, 3)
	scope.Let(slip.Symbol("queue"), gi.Channel(ch))
	defer func() { close(ch) }()
	(&sliptest.Function{
		Scope:  scope,
		Source: `(channel-push queue 7)`,
		Expect: "",
	}).Test(t)
	tt.Equal(t, 1, len(ch))
	result := <-ch
	tt.Equal(t, slip.Fixnum(7), result)
}

func TestChannelPushBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(channel-push)`,
		Panics: true,
	}).Test(t)
}

func TestChannelPushNotChannel(t *testing.T) {
	(&sliptest.Function{
		Source: `(channel-push t 7)`,
		Panics: true,
	}).Test(t)
}
