// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestDeletePackagePackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((pkg (make-package 'delete-test-1)))
                  (delete-package "delete-test-1")
                  (package-name pkg))`,
		Validate: func(t *testing.T, v slip.Object) {
			tt.Nil(t, v)
			tt.Nil(t, slip.FindPackage("delete-test-1"))
		},
	}).Test(t)
}

func TestDeletePackageTwice(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((pkg (make-package 'delete-test-2)))
                  (list (delete-package 'delete-test-2) (delete-package pkg)))`,
		Expect: "(t nil)",
	}).Test(t)
}

func TestDeletePackageNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(delete-package t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestDeletePackageNotFound(t *testing.T) {
	(&sliptest.Function{
		Source:    `(delete-package 'not-found)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestDeletePackageLocked(t *testing.T) {
	(&sliptest.Function{
		Source:    `(delete-package 'bag)`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestDeletePackageInUse(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'delete-test-3)
                  (make-package 'delete-test-4 :use '(delete-test-3))
                  (delete-package "delete-test-3"))`,
		PanicType: slip.PackageErrorSymbol,
	}).Test(t)
}

func TestDeletePackageUser(t *testing.T) {
	(&sliptest.Function{
		Source: `(progn
                  (make-package 'delete-test-5)
                  (make-package 'delete-test-6 :use '(delete-test-5))
                  (delete-package "delete-test-6")
                  (delete-package "delete-test-5"))`,
		Expect: "t",
	}).Test(t)
}
