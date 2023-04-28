// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestListxOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(list* 1)`,
		Expect: "1",
	}).Test(t)
}

func TestListxMultiple(t *testing.T) {
	(&sliptest.Function{
		Source: `(list* 1 2 3)`,
		Expect: "(1 2 . 3)",
	}).Test(t)
}

func TestListxEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(list*)`,
		Panics: true,
	}).Test(t)
}
