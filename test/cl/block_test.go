// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBlockNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestBlockPlain(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (+ 1 2) (+ 2 3))`,
		Expect: "5",
	}).Test(t)
}

func TestBlockReturn(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (+ 1 2) (return-from nil 1) (+ 2 3))`,
		Expect: "1",
	}).Test(t)
}

func TestBlockReturnNamed(t *testing.T) {
	(&sliptest.Function{
		Source: `(block done (+ 1 2) (return-from done 1) (+ 2 3))`,
		Expect: "1",
	}).Test(t)
}

func TestBlockReturnNested(t *testing.T) {
	(&sliptest.Function{
		Source: `(block done (block nil (+ 1 2) (return-from done 1) (+ 2 3)))`,
		Expect: "1",
	}).Test(t)
}

func TestBlockBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(block t (+ 1 2) (return-from done 1) (+ 2 3))`,
		Panics: true,
	}).Test(t)
}

func TestBlockEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(block)`,
		Panics: true,
	}).Test(t)
}

func TestBlockReturnBadParent(t *testing.T) {
	(&sliptest.Function{
		Source: `(let () (block nil (+ 1 2) (return-from done 1) (+ 2 3)))`,
		Panics: true,
	}).Test(t)
}
