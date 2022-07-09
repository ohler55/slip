// Copyright (c) 2022, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/pkg/gi"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeChannelOk(t *testing.T) {
	tf := sliptest.Function{
		Source: `(make-channel 7)`,
		Expect: "#<channel 7>",
	}
	tf.Test(t)
	if ch, ok := tf.Result.(gi.Channel); ok {
		close(ch)
	}
}

func TestMakeChannelBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-channel)`,
		Panics: true,
	}).Test(t)
}

func TestMakeChannelBadArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-channel t)`,
		Panics: true,
	}).Test(t)
}
