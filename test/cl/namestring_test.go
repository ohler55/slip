// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestNamestringOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(namestring "testdata/load-me.lisp")`,
		Expect: `/".+/testdata/load-me.lisp"/`,
	}).Test(t)
}

func TestNamestringBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(namestring t)`,
		Panics: true,
	}).Test(t)
}
