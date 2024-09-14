// Copyright (c) 2024, Peter Ohler, All rights reserved.

package net_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAddressToStringIPv4(t *testing.T) {
	(&sliptest.Function{
		Source: `(address-to-string (coerce #(127 0 0 1) 'octets))`,
		Expect: `"127.0.0.1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(address-to-string '(127 0 0 1))`,
		Expect: `"127.0.0.1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(address-to-string #(127 0 0 1))`,
		Expect: `"127.0.0.1"`,
	}).Test(t)
}

func TestAddressToStringIPv6(t *testing.T) {
	(&sliptest.Function{
		Source: `(address-to-string (coerce #(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1) 'octets))`,
		Expect: `"2003:db7::2:1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(address-to-string '(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1))`,
		Expect: `"2003:db7::2:1"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(address-to-string #(32 3 13 183 0 0 0 0 0 0 0 0 0 2 0 1))`,
		Expect: `"2003:db7::2:1"`,
	}).Test(t)
}

func TestAddressToStringNotAddress(t *testing.T) {
	(&sliptest.Function{
		Source:    `(address-to-string t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestAddressToStringNot4or16(t *testing.T) {
	(&sliptest.Function{
		Source:    `(address-to-string #(127 0 1))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
