// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestNthEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth 0 nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestNthBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth 1 '(a b c))`,
		Expect: "b",
	}).Test(t)
}

func TestNthSetf(t *testing.T) {
	scope := slip.NewScope()
	scope.Let("abc", slip.List{slip.Symbol("a"), slip.Symbol("b"), slip.Symbol("c")})
	(&sliptest.Function{
		Scope:  scope,
		Source: `(setf (nth 1 abc) 7)`,
		Expect: "7",
	}).Test(t)
	tt.Equal(t, "(a 7 c)", slip.ObjectString(scope.Get("abc")))
}

func TestNthOutOfBounds(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth -1 '(a b c))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(nth 3 '(a b c))`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(setf (nth 3 '(a b c)) 7)`,
		Panics: true,
	}).Test(t)
}

func TestNthArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth 1)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(setf (nth 1) 2)`,
		Panics: true,
	}).Test(t)
}

func TestNthBadN(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth t '(a b c))`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(setf (nth t '(a b c)) 7)`,
		Panics: true,
	}).Test(t)
}

func TestNthBadList(t *testing.T) {
	(&sliptest.Function{
		Source: `(nth 1 t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(setf (nth 1 t) 7)`,
		Panics: true,
	}).Test(t)
}

func TestNthSetfBadN(t *testing.T) {
	(&sliptest.Function{
		Source: `(setf (nth t '(a b c) 7)`,
		Panics: true,
	}).Test(t)
}
