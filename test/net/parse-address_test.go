// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestParseAddressIPv4(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-address "127.0.0.1")`,
		Array:  true,
		Expect: "#(127 0 0 1)",
	}).Test(t)
}

func TestParseAddressIPv6(t *testing.T) {
	(&sliptest.Function{
		Source: `(parse-address "2003:db7::2:1")`,
		Array:  true,
		Expect: "#(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1)",
	}).Test(t)
}

func TestParseAddressNotAddress(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-address "123456")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestParseAddressNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(parse-address t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
