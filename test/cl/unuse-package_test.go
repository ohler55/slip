// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnusePackageWithVars(t *testing.T) {
	orig := slip.CurrentPackage
	defer func() {
		scope := slip.NewScope()
		slip.CurrentPackage = orig
		scope.Set("*package*", orig)
		slip.RemovePackage(slip.FindPackage("unuse-test-2"))
		slip.RemovePackage(slip.FindPackage("unuse-test-1"))
	}()
	(&sliptest.Function{
		Source: `(let ((p1 (make-package 'unuse-test-1 :use '(cl)))
                       (p2 (make-package 'unuse-test-2 :use '(cl gi)))
                       result)
                  (in-package p1)
                  (defvar quux1 1)
                  (in-package p2)
                  (addf result (boundp 'quux1))
                  (use-package p1 p2)
                  (addf result (boundp 'quux1))
                  (unuse-package p1 p2)
                  (addf result (boundp 'quux1))
                  result)`,
		Expect: "(nil t nil)",
	}).Test(t)
}

func TestUnusePackageNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unuse-package 'not-found)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(unuse-package 'cl 'not-found)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}
