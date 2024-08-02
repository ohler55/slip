// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDeleteEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 1 nil)",
		Expect: "nil",
	}).Test(t)
}

func TestDeleteListPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c b d))",
		Expect: "(a c d)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'x '(a b c))",
		Expect: "(a b c)",
	}).Test(t)
}

func TestDeleteListCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c b d) :count 1)",
		Expect: "(a c b d)",
	}).Test(t)
}

func TestDeleteListFromEndCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c b d) :from-end t :count 1)",
		Expect: "(a b c d)",
	}).Test(t)
}

func TestDeleteListStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c b d) :start 1 :end 3)",
		Expect: "(a c b d)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'b '(a b c b d) :start 1 :end nil)",
		Expect: "(a c d)",
	}).Test(t)
}

func TestDeleteListKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete nil '(1 2 3 4 5 6) :key 'evenp)",
		Expect: "(2 4 6)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete nil '(1 2 3 4 5 6) :key 'evenp :from-end t :count 2)",
		Expect: "(1 2 4 6)",
	}).Test(t)
}

func TestDeleteListTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 3 '(1 2 3 4 5 6) :test '<)",
		Expect: "(1 2 3)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 3 '(1 2 3 4 5 6) :test '< :from-end t :count 2)",
		Expect: "(1 2 3 4)",
	}).Test(t)
}

func TestDeleteVectorPlain(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b #(a b c b d))",
		Array:  true,
		Expect: "#(a c d)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'x #(a b c))",
		Array:  true,
		Expect: "#(a b c)",
	}).Test(t)
}

func TestDeleteStringPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete #\B "ABCBD")`,
		Expect: `"ACD"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete #\X "ABCBD")`,
		Expect: `"ABCBD"`,
	}).Test(t)
}

func TestDeleteStringCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete #\B "ABCBD" :count 1)`,
		Expect: `"ACBD"`,
	}).Test(t)
}

func TestDeleteStringFromEndCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete #\B "ABCBD" :count 1 :from-end t)`,
		Expect: `"ABCD"`,
	}).Test(t)
}

func TestDeleteStringStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete #\B "ABCBD" :start 1 :end 3)`,
		Expect: `"ACBD"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete #\B "ABCBD" :start 1 :end nil)`,
		Expect: `"ACD"`,
	}).Test(t)
}

func TestDeleteStringKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete 66 "ABCBD" :key 'char-code)`,
		Expect: `"ACD"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete 66 "ABCBD" :key 'char-code :from-end t :count 1)`,
		Expect: `"ABCD"`,
	}).Test(t)
}

func TestDeleteStringTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete 66 "ABCBD" :key 'char-code :test '<)`,
		Expect: `"ABB"`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete 66 "ABCBD" :key 'char-code :test '< :from-end t :count 1)`,
		Expect: `"ABCB"`,
	}).Test(t)
}

func TestDeleteOctetsCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete (coerce #\B 'octet) (coerce "ABCBD" 'octets) :count 1)`,
		Array:  true,
		Expect: "#(65 67 66 68)",
	}).Test(t)
}

func TestDeleteOctetsFromEndCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete (coerce #\B 'octet) (coerce "ABCBD" 'octets) :count 1 :from-end t)`,
		Array:  true,
		Expect: "#(65 66 67 68)",
	}).Test(t)
}

func TestDeleteOctetsStartEnd(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete (coerce #\B 'octet) (coerce "ABCBD" 'octets) :start 1 :end 3)`,
		Array:  true,
		Expect: "#(65 67 66 68)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete (coerce #\B 'octet) (coerce "ABCBD" 'octets) :start 1 :end nil)`,
		Array:  true,
		Expect: "#(65 67 68)",
	}).Test(t)
}

func TestDeleteOctetsKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete 66 (coerce "ABCBD" 'octets) :key (lambda (n) (coerce n 'fixnum)))`,
		Array:  true,
		Expect: "#(65 67 68)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete 66 (coerce "ABCBD" 'octets) :key (lambda (n) (coerce n 'fixnum)) :count 1 :from-end t)`,
		Array:  true,
		Expect: "#(65 66 67 68)",
	}).Test(t)
}

func TestDeleteOctetsTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(delete 66 (coerce "ABCBD" 'octets) :key (lambda (n) (coerce n 'fixnum)) :test '<)`,
		Array:  true,
		Expect: "#(65 66 66)",
	}).Test(t)
	(&sliptest.Function{
		Source: `(delete 66 (coerce "ABCBD" 'octets)
                        :key (lambda (n) (coerce n 'fixnum)) :test '< :from-end t :count 1)`,
		Array:  true,
		Expect: "#(65 66 67 66)",
	}).Test(t)
}

func TestDeleteNotSequence(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b t)",
		Panics: true,
	}).Test(t)
}

func TestDeleteBadStart(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :start t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :start -1)",
		Panics: true,
	}).Test(t)
}

func TestDeleteBadEnd(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :end t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :end -1)",
		Panics: true,
	}).Test(t)
}

func TestDeleteBadCount(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :count t)",
		Panics: true,
	}).Test(t)
}

func TestDeleteBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) t 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :bad 3)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(delete 'b '(a b c) :count)",
		Panics: true,
	}).Test(t)
}
