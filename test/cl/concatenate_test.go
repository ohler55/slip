// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestConcatenateList(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'list "abc" '(d e f) #(1 2 3) (coerce '(4 5 6) 'octets))`,
		Expect: `(#\a #\b #\c d e f 1 2 3 4 5 6)`,
	}).Test(t)
}

func TestConcatenateString(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'string "abc" '(#\d #\e #\f) #(#\g #\h #\i) (coerce "jk" 'octets))`,
		Expect: `"abcdefghijk"`,
	}).Test(t)
}

func TestConcatenateVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'vector "abc" '(d e f) #(1 2 3))`,
		Array:  true,
		Expect: `#(#\a #\b #\c d e f 1 2 3)`,
	}).Test(t)
}

func TestConcatenateOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(concatenate 'octets "abc" '(#\d "e" 102) #(1 2 3) (coerce #(4 5) 'octets) (list (coerce 6 'octet)))`,
		Array:  true,
		Expect: `#(97 98 99 100 101 102 1 2 3 4 5 6)`,
	}).Test(t)
}

func TestConcatenateBadResultType(t *testing.T) {
	(&sliptest.Function{
		Source:    `(concatenate 'fixnum "abc" '(d e f) #(1 2 3))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestConcatenateBadList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(concatenate 'list t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestConcatenateBadString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(concatenate 'string t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(concatenate 'string '(#\a t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestConcatenateBadVector(t *testing.T) {
	(&sliptest.Function{
		Source:    `(concatenate 'vector t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestConcatenateNotOctet(t *testing.T) {
	(&sliptest.Function{
		Source:    `(concatenate 'octets t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(concatenate 'octets '(t))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(concatenate 'octets '(300))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
