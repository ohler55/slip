// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMakeSocketOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(send (make-socket :domain :inet :type :stream :protocol :ip) :type)`,
		Expect: ":stream",
	}).Test(t)
}
