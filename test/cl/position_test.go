// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPositionEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 1 nil)",
		Expect: "nil",
	}).Test(t)
}

func TestPositionListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b '(a b c))",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'x '(a b c))",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 3 '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestPositionVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b #(a b c))",
		Expect: "1",
	}).Test(t)
}

func TestPositionStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(position #\b "abc")`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position #\x "abc")`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position 3 "abc")`,
		Expect: "nil",
	}).Test(t)
}

func TestPositionOctetsPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(position (coerce #\b 'octet) (coerce "abc" 'octets))`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position (coerce #\x 'octet) (coerce "abc" 'octets))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position 3 (coerce "abc" 'octets))`,
		Expect: "nil",
	}).Test(t)
}

func TestPositionNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b t)",
		Panics: true,
	}).Test(t)
}

func TestPositionListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'c '(a b c d e) :start 1 :end 3)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'd '(a b c d e) :start 1 :end 3)",
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'd '(a b c d e) :start 5 :end nil)",
		Expect: "nil",
	}).Test(t)
}

func TestPositionBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'b '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestPositionStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(position #\c "abcde" :start 1 :end 3)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position #\d "abcde" :start 1 :end 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position #\d "abcde" :start 5 :end nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestPositionOctetsStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(position (coerce #\c 'octet) (coerce "abcde" 'octets) :start 1 :end 3)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position (coerce #\d 'octet) (coerce "abcde" 'octets) :start 1 :end 3)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position (coerce #\d 'octet) (coerce "abcde" 'octets) :start 5 :end nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestPositionListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b '((a 1) (b 2) (c 3) (b 4)) :key 'car)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'b '((a 1) (b 2) (c 3) (b 4)) :key 'car :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestPositionStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(position 66 "ABC" :key 'char-code)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position 66 "ABC" :key 'char-code :from-end t)`,
		Expect: "1",
	}).Test(t)
}

func TestPositionOctetsKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(position 66 (coerce "ABC" 'octets) :key (lambda (x) (coerce x 'fixnum)))`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position 66 (coerce "ABC" 'octets) :key (lambda (x) (coerce x 'fixnum)) :from-end t)`,
		Expect: "1",
	}).Test(t)
}

func TestPositionListTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 2 '(1 2 3 4) :test '<)",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 2 '(1 2 3 4) :test '< :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestPositionStringTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(position 66 "ABCD" :test '< :key 'char-code)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position 66 "ABCD" :test '< :key 'char-code :from-end t)`,
		Expect: "3",
	}).Test(t)
}

func TestPositionOctetsTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(position 66 (coerce "ABCD" 'octets) :test '<)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(position 66 (coerce "ABCD" 'octets) :test '< :from-end t)`,
		Expect: "3",
	}).Test(t)
}

func TestPositionBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'b '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestPositionBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(position 'b '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'b '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(position 'b '(a b c) :count 3)",
		Panics: true,
	}).Test(t)
}
