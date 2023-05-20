// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPathnameNameSimple(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-name "three.lisp")`,
		Expect: `"three"`,
	}).Test(t)
}

func TestPathnameNameAbsolute(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-name "/one/two/three.lisp")`,
		Expect: `"three"`,
	}).Test(t)
}

func TestPathnameNameNoType(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-name "/one/two/three")`,
		Expect: `"three"`,
	}).Test(t)
}

func TestPathnameNameBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-name t)`,
		Panics: true,
	}).Test(t)
}
