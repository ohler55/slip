// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeInet6AddressIPv4(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-inet6-address "127.0.0.1")`,
		Array:     true,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMakeInet6AddressIPv6(t *testing.T) {
	(&sliptest.Function{
		Source: `(make-inet6-address "2003:db7::2:1")`,
		Array:  true,
		Expect: "#(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1)",
	}).Test(t)
}

func TestMakeInet6AddressNotAddress(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-inet6-address "123456")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestMakeInet6AddressNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-inet6-address t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
