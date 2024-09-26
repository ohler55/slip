// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeInetAddressIPv4(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-inet-address "127.0.0.1")`,
		Array:  true,
		Expect: "#(127 0 0 1)",
	}).Test(t)
}

func TestMakeInetAddressIPv6(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-inet-address "2003:db7::2:1")`,
		Array:     true,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMakeInetAddressNotAddress(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-inet-address "123456")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMakeInetAddressNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-inet-address t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
