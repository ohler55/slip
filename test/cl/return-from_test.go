// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestReturnFromOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (+ 1 2) (return-from nil 1) (+ 2 3))`,
		Expect: "1",
	}).Test(t)
}

func TestReturnFromEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (return-from) (+ 2 3))`,
		Panics: true,
	}).Test(t)
}

func TestReturnFromBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(block nil (return-from t 1) (+ 2 3))`,
		Panics: true,
	}).Test(t)
}

func TestReturnFromNotInBlock(t *testing.T) {
	(&sliptest.Function{
		Source: `(return-from nil 1)`,
		Panics: true,
	}).Test(t)
}
