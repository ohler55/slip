// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestCountEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 1 nil)",
		Expect: "0",
	}).Test(t)
}

func TestCountListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b '(a b c b d))",
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 'x '(a b c))",
		Expect: "0",
	}).Test(t)
}

func TestCountListFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b '(a b c b d) :from-end t)",
		Expect: "2",
	}).Test(t)
}

func TestCountListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b '(a b c b d) :start 1 :end 3)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 'b '(a b c b d) :start 1 :end 3 :from-end t)",
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 'b '(a b c b d) :start 1 :end nil)",
		Expect: "2",
	}).Test(t)
}

func TestCountListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(count nil '(1 2 3 4 5 6) :key 'evenp)",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count nil '(1 2 3 4 5 6) :key 'evenp :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestCountListTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 3 '(1 2 3 4 5 6) :test '<)",
		Expect: "3",
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 3 '(1 2 3 4 5 6) :test '< :from-end t)",
		Expect: "3",
	}).Test(t)
}

func TestCountVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b #(a b c b d))",
		Expect: "2",
	}).Test(t)
}

func TestCountStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(count #\B "ABCBD")`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count #\X "ABCBD")`,
		Expect: "0",
	}).Test(t)
}

func TestCountOctetsPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(count (coerce 98 'octet) (coerce "abcb" 'octets))`,
		Expect: "2",
	}).Test(t)
}

func TestCountStringFromEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(count #\B "ABCBD" :from-end t)`,
		Expect: "2",
	}).Test(t)
}

func TestCountStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(count #\B "ABCBD" :start 1 :end 3)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count #\B "ABCBD" :start 1 :end 3 :from-end t)`,
		Expect: "1",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count #\B "ABCBD" :start 1 :end nil)`,
		Expect: "2",
	}).Test(t)
}

func TestCountStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(count 66 "ABCBD" :key 'char-code)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count 66 "ABCBD" :key 'char-code :from-end t)`,
		Expect: "2",
	}).Test(t)
}

func TestCountStringTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(count 66 "ABCBD" :key 'char-code :test '<)`,
		Expect: "2",
	}).Test(t)
	(&sliptest.Function{
		Source: `(count 66 "ABCBD" :key 'char-code :test '< :from-end t)`,
		Expect: "2",
	}).Test(t)
}

func TestCountNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b t)",
		Panics: true,
	}).Test(t)
}

func TestCountBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 'b '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestCountBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 'b '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestCountBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(count 'b '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(count 'b '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
}
