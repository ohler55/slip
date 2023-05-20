// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestDirectoryNamestring(t *testing.T) {
	(&sliptest.Function{
		Source: `(directory-namestring "../one/two/three.lisp")`,
		Expect: `"../one/two"`,
	}).Test(t)
}

func TestDirectoryNamestringBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(directory-namestring t)`,
		Panics: true,
	}).Test(t)
}
