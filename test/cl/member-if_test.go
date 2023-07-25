// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMemberIfNil(t *testing.T) {
	(&sliptest.Function{
		Source: "(member-if 'null nil)",
		Expect: "nil",
	}).Test(t)
}

func TestMemberIfBasic(t *testing.T) {
	(&sliptest.Function{
		Source: "(member-if 'null '(a nil c))",
		Expect: "(nil c)",
	}).Test(t)
	(&sliptest.Function{
		Source: "(member-if 'null '(a b c))",
		Expect: "nil",
	}).Test(t)
}

func TestMemberIfKey(t *testing.T) {
	(&sliptest.Function{
		Source: "(member-if 'null '((a . 1) (nil . 2) (c . 3)) :key 'car)",
		Expect: "((nil . 2) (c . 3))",
	}).Test(t)
}

func TestMemberIfNotList(t *testing.T) {
	(&sliptest.Function{
		Source: "(member-if 'null t)",
		Panics: true,
	}).Test(t)
}

func TestMemberIfNotKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: "(member-if 'null '(a b c) t t)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(member-if 'null '(a b c) :key)",
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: "(member-if 'null '(a b c) :bad t)",
		Panics: true,
	}).Test(t)
}
