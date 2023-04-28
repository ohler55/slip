// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestAdjoinNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin 3 nil)`,
		Expect: "(3)",
	}).Test(t)
}

func TestAdjoinList(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin 3 '(2 1))`,
		Expect: "(3 2 1)",
	}).Test(t)
}

func TestAdjoinHas(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin 3 '(1 2 3))`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestAdjoinKeyTest(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin '(a . 1) '((a . 2) (b . 2)) :key 'cdr :test 'equal)`,
		Expect: "((a . 1) (a . 2) (b . 2))",
	}).Test(t)
}

func TestAdjoinKeyTestHas(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin '(x . 2) '((a . 2) (b . 2)) :key 'cdr :test 'equal)`,
		Expect: "((a . 2) (b . 2))",
	}).Test(t)
}

func TestAdjoinNotList(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin 3 t)`,
		Panics: true,
	}).Test(t)
}

func TestAdjoinBadKeyword(t *testing.T) {
	(&sliptest.Function{
		Source: `(adjoin 3 '() :bad t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(adjoin 3 '() "bad" t)`,
		Panics: true,
	}).Test(t)
}
