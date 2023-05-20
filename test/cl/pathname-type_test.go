// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPathnameTypeSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-type "three.lisp")`,
		Expect: `"lisp"`,
	}).Test(t)
}

func TestPathnameTypeAbsolute(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-type "/one/two/three.lisp")`,
		Expect: `"lisp"`,
	}).Test(t)
}

func TestPathnameTypeNoType(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-type "/one/two/three")`,
		Expect: "nil",
	}).Test(t)
}

func TestPathnameTypeBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-type t)`,
		Panics: true,
	}).Test(t)
}
