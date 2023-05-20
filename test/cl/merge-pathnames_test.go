// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestMergePathnamesOne(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge-pathnames "three.lisp")`,
		Expect: `/".*test/cl/three.lisp"/`,
	}).Test(t)
}

func TestMergePathnamesTwo(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge-pathnames "three.lisp" "one/two")`,
		Expect: `"one/two/three.lisp"`,
	}).Test(t)
}

func TestMergePathnamesNotString(t *testing.T) {
	(&sliptest.Function{
		Source: `(merge-pathnames "three.lisp" t)`,
		Panics: true,
	}).Test(t)
	(&sliptest.Function{
		Source: `(merge-pathnames t)`,
		Panics: true,
	}).Test(t)
}
