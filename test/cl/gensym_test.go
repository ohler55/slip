// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestGensymBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(gensym)`,
		Expect: "/g[0-9]+/",
	}).Test(t)
}

func TestGensymPrefix(t *testing.T) {
	(&sliptest.Function{
		Source: `(gensym "x")`,
		Expect: "/x[0-9]+/",
	}).Test(t)
}

func TestGensymSuffix(t *testing.T) {
	(&sliptest.Function{
		Source: `(gensym 123)`,
		Expect: "g123",
	}).Test(t)
}

func TestGensymCounter(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*gensym-counter* 222)) (gensym))`,
		Expect: "g222",
	}).Test(t)
}

func TestGensymBadCounter(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((*gensym-counter* 1.23)) (gensym))`,
		Panics: true,
	}).Test(t)
}

func TestGensymNegativeSuffix(t *testing.T) {
	(&sliptest.Function{
		Source: `(gensym -123)`,
		Panics: true,
	}).Test(t)
}

func TestGensymBadX(t *testing.T) {
	(&sliptest.Function{
		Source: `(gensym 'abc)`,
		Panics: true,
	}).Test(t)
}
