// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCharLengthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-length nil)`,
		Expect: "0",
	}).Test(t)
}

func TestCharLengthString(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-length "TEST_ぴ_" :end 6)`,
		Expect: "6",
	}).Test(t)
}

func TestCharLengthOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-length (coerce #(84 69 83 84 95 227 129 180) 'octets))`,
		Expect: "6",
	}).Test(t)
}

func TestCharLengthList(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-length '(#\a #\Space #\T #\e #\s #\t #\s) :start 2 :end 6)`,
		Expect: "4",
	}).Test(t)
}

func TestCharLengthVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(char-length (vector #\a #\Space #\T #\e #\s #\t #\s) :start 2 :end 6)`,
		Expect: "4",
	}).Test(t)
}

func TestCharLengthBadArg(t *testing.T) {
	(&sliptest.Function{
		Source:    `(char-length 2)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCharLengthBadList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(char-length '(#\a t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestCharLengthBadStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source:    `(char-length "TEST_ぴ_" :end 8)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(char-length "TEST_ぴ_" :start 8)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(char-length "TEST_ぴ_" :start 4 :end 2)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(char-length "TEST_ぴ_" :start t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(char-length "TEST_ぴ_" :end  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
