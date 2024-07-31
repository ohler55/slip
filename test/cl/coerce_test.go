// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCoerceTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3 t)`,
		Expect: "3",
	}).Test(t)
}

func TestCoerceToList(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'list)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a b c) 'list)`,
		Expect: "(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(a b c) 'list)`,
		Expect: "(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'list)`,
		Expect: `(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'list)`,
		Expect: `(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (coerce #(1 2 3) 'octets) 'list)`,
		Expect: "(1 2 3)",
	}).Test(t)

	(&sliptest.Function{
		Source: `(coerce 3 'list)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToVector(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'vector)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a b c) 'vector)`,
		Array:  true,
		Expect: "#(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(a b c) 'vector)`,
		Array:  true,
		Expect: "#(a b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'vector)`,
		Array:  true,
		Expect: `#(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'vector)`,
		Array:  true,
		Expect: `#(#\a #\b #\c)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'vector)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToOctets(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'octets)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(97 98 99) 'octets)`,
		Array:  true,
		Expect: "#(97 98 99)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(#\a #\b #\c) 'octets)`,
		Array:  true,
		Expect: "#(97 98 99)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'octets)`,
		Array:  true,
		Expect: `#(97 98 99)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'octets)`,
		Array:  true,
		Expect: `#(97 98 99)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'octets)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToString(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce nil 'string)`,
		Expect: `""`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 'abc 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(#\a #\b #\c) 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #(#\a #\b #\c) 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (coerce "abc" 'octets) 'string)`,
		Expect: `"abc"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'string)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(#\a b) 'string)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToCharacter(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce #\A 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (coerce 65 'octet) 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce -1 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65.0 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65.3 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 130/2 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 130/3 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(65 0) 'character)`,
		Expect: `#\A`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(65 1) 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce nil 'character)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a) 'character)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToOctet(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce #\A 'octet)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65 'octet)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (coerce 65 'octet) 'octet)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce -1 'octet)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65.0 'octet)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 65.3 'octet)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0L+0 'octet)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 130/2 'octet)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 130/3 'octet)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(65 0) 'octet)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(65 1) 'octet)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce nil 'octet)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(a) 'octet)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (- 30000000000000000123 30000000000000000000) 'octet)`,
		Expect: `123`,
	}).Test(t)
}

func TestCoerceToInteger(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 123 'integer)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'integer)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0L+0 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1L+0 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'integer)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'integer)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (- 30000000000000000123 30000000000000000000) 'integer)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'integer)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToFixnum(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 123 'fixnum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (coerce 123 'octet) 'fixnum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'fixnum)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0L+0 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1L+0 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'fixnum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (- 30000000000000000123 30000000000000000000) 'fixnum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 30000000000000000123 'fixnum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'fixnum)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToBignum(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 30000000000000000123 'bignum)`,
		Expect: `30000000000000000123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 123 'bignum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (coerce 123 'octet) 'bignum)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'bignum)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'bignum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1 'bignum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 50000000000000000000000000.0L+0 'bignum)`,
		Expect: `50000000000000000000000000`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.1L+0 'bignum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'bignum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'bignum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'bignum)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'bignum)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'bignum)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 30000000000000000000 'float)`,
		Expect: `3e+19`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 123 'float)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'float)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 50000000000000000000000000.0L+0 'float)`,
		Expect: `5e+25`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.2L+0 'float)`,
		Expect: `5.2`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'float)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'float)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToSingleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 30000000000000000000 'single-float)`,
		Expect: `3e+19`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 123 'single-float)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'single-float)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'single-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0s+0 'single-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 50000000000000000000000000.0L+0 'single-float)`,
		Expect: `5e+25`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.5L+0 'single-float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'single-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'single-float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'single-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'single-float)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'single-float)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToDoubleFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 30000000000000000000 'double-float)`,
		Expect: `3e+19`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 123 'double-float)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'double-float)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'double-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 50000000000000000000000000.0L+0 'double-float)`,
		Expect: `5e+25`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.5L+0 'double-float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'double-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'double-float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'double-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'double-float)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'double-float)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToLongFloat(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 30000000000000000000 'long-float)`,
		Expect: `3e+19`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 123 'long-float)`,
		Expect: `123`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'long-float)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0s+0 'long-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.0 'long-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 50000000000000000000000000.0L+0 'long-float)`,
		Expect: `5e+25`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5.5L+0 'long-float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 10/2 'long-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 11/2 'long-float)`,
		Expect: `5.5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'long-float)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'long-float)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'long-float)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToRational(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3 'rational)`,
		Expect: `3`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3.5 'rational)`,
		Expect: `7/2`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'rational)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'rational)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'rational)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'rational)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToRatio(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3/2 'ratio)`,
		Expect: `3/2`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'ratio)`,
		Expect: `3`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3.5 'ratio)`,
		Expect: `7/2`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'ratio)`,
		Expect: `65`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 0) 'ratio)`,
		Expect: `5`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #C(5 1) 'ratio)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(5 1) 'ratio)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToComplex(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce #C(1 2) 'complex)`,
		Expect: `#C(1 2)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3 'complex)`,
		Expect: `#C(3 0)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce #\A 'complex)`,
		Expect: `#C(65 0)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 3.5 'complex)`,
		Expect: `#C(3.5 0)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '(1 2) 'complex)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 'abc 'symbol)`,
		Expect: `abc`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce "abc" 'symbol)`,
		Expect: `abc`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 5 'symbol)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToAssoc(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce '((a . 1) nil) 'assoc)`,
		Expect: `((a . 1) nil)`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '((a . 1) (b 2)) 'assoc)`,
		Expect: `((a . 1) (b 2))`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (let ((table (make-hash-table))) (setf (gethash 'a table) 1) table) 'assoc)`,
		Expect: `((a . 1))`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '((a . 1) 2) 'assoc)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 7 'assoc)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToHashTable(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce '((a . 1) nil) 'hash-table)`,
		Expect: `#<hash-table eql 1/-->`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '((a . 1) (b 2)) 'hash-table)`,
		Expect: `#<hash-table eql 2/-->`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (let ((table (make-hash-table))) (setf (gethash 'a table) 1) table) 'hash-table)`,
		Expect: `#<hash-table eql 1/-->`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce '((a . 1) 2) 'hash-table)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 7 'hash-table)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceToFunction(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 'car 'function)`,
		Expect: `#<function car>`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce (lambda () nil) 'function)`,
		Expect: `/#<function \(lambda \(\)\) {[0-9a-f]+}>/`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(coerce 7 'function)`,
		Panics: true,
	}).Test(t)
}

func TestCoerceBadType(t *testing.T) {
	(&sliptest.Function{
		Source: `(coerce 3 5)`,
		Panics: true,
	}).Test(t)
}
