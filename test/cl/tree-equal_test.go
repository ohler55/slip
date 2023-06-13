// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestTreeEqualSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(tree-equal 5 5)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(tree-equal 5 5.1)`,
		Expect: "nil",
	}).Test(t)
}

func TestTreeEqualTree(t *testing.T) {
	(&sliptest.Function{
		Source: `(tree-equal '(a (b c)) '(a (b c)))`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(tree-equal '(a (b c)) '(a (b d)))`,
		Expect: "nil",
	}).Test(t)
}

func TestTreeEqualTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(tree-equal 5 5 :test '=)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(tree-equal 5 6 :test '=)`,
		Expect: "nil",
	}).Test(t)
}

func TestTreeEqualBadTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(tree-equal 5 5 :test t)`,
		Panics: true,
	}).Test(t)
}

func TestTreeEqualBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(tree-equal 5 5 :bad t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(tree-equal 5 5 t t)`,
		Panics: true,
	}).Test(t)
}

func TestTreeEqualNoKeywordValue(t *testing.T) {
	(&sliptest.Function{
		Source: `(tree-equal 5 5 :test)`,
		Panics: true,
	}).Test(t)
}
