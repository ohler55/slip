// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPathnameDirectoryNone(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-directory "three.lisp")`,
		Expect: `nil`,
	}).Test(t)
}

func TestPathnameDirectoryAbsolute(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-directory "/one/two/three.lisp")`,
		Expect: `(:absolute "one" "two")`,
	}).Test(t)
}

func TestPathnameDirectoryRelative(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-directory "../one/two/three.lisp")`,
		Expect: `(:relative :up "one" "two")`,
	}).Test(t)
}

func TestPathnameDirectoryBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(pathname-directory t)`,
		Panics: true,
	}).Test(t)
}
