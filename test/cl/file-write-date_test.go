// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFileWriteDateFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-write-date "testdata/load-me.lisp")`,
		Expect: `/@.+/`, // could verify it is a time
	}).Test(t)
}

func TestFileWriteDateNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-write-date "testdata/not-me.lisp")`,
		Panics: true,
	}).Test(t)
}

func TestFileWriteDateNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-write-date t)`,
		Panics: true,
	}).Test(t)
}
