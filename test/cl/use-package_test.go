// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUsePackageWithVars(t *testing.T) {
	orig := slip.CurrentPackage
	defer func() {
		scope := slip.NewScope()
		slip.CurrentPackage = orig
		scope.Set("*package*", orig)
		slip.RemovePackage(slip.FindPackage("use-test-2"))
		slip.RemovePackage(slip.FindPackage("use-test-1"))
	}()
	(&sliptest.Function{
		Source: `(let ((p1 (make-package 'use-test-1 :use '(cl)))
                       (p2 (make-package 'use-test-2 :use '(cl gi)))
                       result)
                  (in-package p1)
                  (defvar quux1 1)
                  (export 'quux1)
                  (in-package p2)
                  (addf result (boundp 'quux1))
                  (use-package p1 p2)
                  (addf result (boundp 'quux1))
                  result)`,
		Expect: "(nil t)",
	}).Test(t)
}

func TestUsePackageNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(use-package 'not-found)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(use-package 'cl 'not-found)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}
