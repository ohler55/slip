// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFindPackageString(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-package "User")`,
		Expect: `#<package "common-lisp-user">`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-package "not-a-package")`,
		Expect: "nil",
	}).Test(t)
}

func TestFindPackageSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-package 'User)`,
		Expect: `#<package "common-lisp-user">`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(find-package 'not-a-package)`,
		Expect: "nil",
	}).Test(t)
}

func TestFindPackageArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-package)`,
		Panics: true,
	}).Test(t)
}

func TestFindPackageBadName(t *testing.T) {
	(&sliptest.Function{
		Source: `(find-package t)`,
		Panics: true,
	}).Test(t)
}
