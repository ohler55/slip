// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFileNamestring(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-namestring "../one/two/three.lisp")`,
		Expect: `"three.lisp"`,
	}).Test(t)
}

func TestFileNamestringBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-namestring t)`,
		Panics: true,
	}).Test(t)
}
