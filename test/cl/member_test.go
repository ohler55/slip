// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMemberNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(member 'b nil)",
		Expect: "nil",
	}).Test(t)
}

func TestMemberBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(member 'b '(a b c))",
		Expect: "(b c)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(member 'x '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestMemberKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(member 'b '((a . 1) (b . 2) (c . 3)) :key 'car)",
		Expect: "((b . 2) (c . 3))",
	}).Test(t)
}

func TestMemberTest(t *testing.T) {
	(&sliptest.Function{
		Source: "(member 1 '(1 2 3) :test '<)",
		Expect: "(2 3)",
	}).Test(t)
}

func TestMemberNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(member 'b t)",
		Panics: true,
	}).Test(t)
}

func TestMemberNotKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(member 'b '(a b c) t t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(member 'b '(a b c) :key)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(member 'b '(a b c) :bad t)",
		Panics: true,
	}).Test(t)
}
