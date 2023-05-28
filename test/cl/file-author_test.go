// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFileAuthorFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-author "testdata/load-me.lisp")`,
		Expect: `/".+"/`,
	}).Test(t)
}

func TestFileAuthorNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-author "testdata/not-me.lisp")`,
		Expect: `nil`,
	}).Test(t)
}

func TestFileAuthorBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-author t)`,
		Panics: true,
	}).Test(t)
}
