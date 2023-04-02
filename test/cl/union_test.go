// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestUnionNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(union nil nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestUnionListNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b c b) nil)`,
		Expect: "(a b c)",
	}).Test(t)
}

func TestUnionListList(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b c b) '(d b e))`,
		Expect: "(a b c d e)",
	}).Test(t)
}

func TestUnionTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b c b) '(d b e) :test 'equal)`,
		Expect: "(a b c d e)",
	}).Test(t)
}

func TestUnionKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b 1 b) '(d 2 e) :key 'symbolp)`,
		Expect: "(a 1)",
	}).Test(t)
}

func TestUnionNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(union t '(d b e))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(union '(a b c) t)`,
		Panics: true,
	}).Test(t)
}

func TestUnionKeywordNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b) '(b c) t)`,
		Panics: true,
	}).Test(t)
}

func TestUnionBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b) '(b c) :not-valid t)`,
		Panics: true,
	}).Test(t)
}

func TestUnionNoKeywordArg(t *testing.T) {
	(&sliptest.Function{
		Source: `(union '(a b) '(b c) :key)`,
		Panics: true,
	}).Test(t)
}
