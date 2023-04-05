// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestIntersectionNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(intersection nil nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestIntersectionListNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(intersection '(a b) nil)`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(intersection nil '(a b))`,
		Expect: "nil",
	}).Test(t)
}

func TestIntersectionListBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(intersection '(a b) '(b c))`,
		Expect: "(b)",
	}).Test(t)
}

func TestIntersectionListDup(t *testing.T) {
	(&sliptest.Function{
		Source: `(intersection '(a b c b a) '(b c d c b))`,
		Expect: "(b c)",
	}).Test(t)
}

func TestIntersectionKey(t *testing.T) {
	(&sliptest.Function{
		Source: `(intersection '(1 2 3) '(4 5 6) :key 'evenp)`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestIntersectionTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(intersection '(a b c d c) '(c b) :test 'equal)`,
		Expect: "(b c)",
	}).Test(t)
}
