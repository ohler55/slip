// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestInPackageString(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	defer scope.Set("*package*", orig)
	(&sliptest.Function{
		Source: `(in-package "User")`,
		Expect: `#<package common-lisp-user>`,
	}).Test(t)
	p := scope.Get("*package*")
	tt.Equal(t, "#<package common-lisp-user>", slip.ObjectString(p))
}

func TestInPackageSymbol(t *testing.T) {
	scope := slip.NewScope()
	orig := scope.Get("*package*")
	defer scope.Set("*package*", orig)
	(&sliptest.Function{
		Source: `(in-package 'user)`,
		Expect: `#<package common-lisp-user>`,
	}).Test(t)
	p := scope.Get("*package*")
	tt.Equal(t, "#<package common-lisp-user>", slip.ObjectString(p))
}

func TestInPackageArgCount(t *testing.T) {
	(&sliptest.Function{
		Source:    `(in-package)`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}

func TestInPackageBadName(t *testing.T) {
	(&sliptest.Function{
		Source:    `(in-package t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(in-package 'quux)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}
