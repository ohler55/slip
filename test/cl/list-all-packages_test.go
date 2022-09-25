// Copyright (c) 2022, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestListAllPackagesOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(list-all-packages)`,
		Expect: `(#<package "bag"> #<package "common-lisp"> #<package "common-lisp-user"> ` +
			`#<package "flavors"> #<package "gi">)`,
	}).Test(t)
}

func TestListAllPackagesBadArgCount(t *testing.T) {
	(&sliptest.Function{
		Source: `(list-all-packages t)`,
		Panics: true,
	}).Test(t)
}
