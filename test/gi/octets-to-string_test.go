// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOctetsToStringList(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string '(84 69 83 84 95 86 65 82))`,
		Expect: `"TEST_VAR"`,
	}).Test(t)
}

func TestOctetsToStringOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string (coerce '(84 69 83 84 95 86 65 82) 'octets))`,
		Expect: `"TEST_VAR"`,
	}).Test(t)
}

func TestOctetsToStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string #(84 69 83 #\T 95 86 65 82):start 1 :end 7)`,
		Expect: `"EST_VA"`,
	}).Test(t)
}

func TestOctetsToStringEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(octets-to-string '())`,
		Expect: `""`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(octets-to-string nil)`,
		Expect: `""`,
	}).Test(t)
}

func TestOctetsToStringBadOctets(t *testing.T) {
	(&sliptest.Function{
		Source:    `(octets-to-string 4)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octets-to-string '(t))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestOctetsToStringBadList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(octets-to-string '("foo" "bar"))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestOctetsToStringBadStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(octets-to-string #(84 69 83 84) :start -1)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octets-to-string #(84 69 83 84) :end t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octets-to-string #(84 69 83 84) :end 1 :start 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octets-to-string #(84 69 83 84) :start 5)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octets-to-string #(84 69 83 84) :end 5)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
