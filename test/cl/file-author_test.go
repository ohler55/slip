// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
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
		Source:    `(file-author "testdata/not-me.lisp")`,
		PanicType: slip.FileErrorSymbol,
	}).Test(t)
}

func TestFileAuthorBadPath(t *testing.T) {
	(&sliptest.Function{
		Source:    `(file-author t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
