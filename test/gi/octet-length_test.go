// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestOctetLengthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(octet-length nil)`,
		Expect: "0",
	}).Test(t)
}

func TestOctetLengthString(t *testing.T) {
	(&sliptest.Function{
		Source: `(octet-length "TEST_ぴ_" :end 6)`,
		Expect: "8",
	}).Test(t)
}

func TestOctetLengthList(t *testing.T) {
	(&sliptest.Function{
		Source: `(octet-length '(#\a #\Space #\T #\e #\s #\t #\s) :start 2 :end 6)`,
		Expect: "4",
	}).Test(t)
}

func TestOctetLengthVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(octet-length (vector #\a #\Space #\T #\e #\s #\t #\s) :start 2 :end 6)`,
		Expect: "4",
	}).Test(t)
}

func TestOctetLengthBadArg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(octet-length 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestOctetLengthBadList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(octet-length '(#\a t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestOctetLengthBadStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(octet-length "TEST_ぴ_" :end 8)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octet-length "TEST_ぴ_" :start 8)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octet-length "TEST_ぴ_" :start 4 :end 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octet-length "TEST_ぴ_" :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(octet-length "TEST_ぴ_" :end  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
