// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestUnlockPackagePackage(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("unlock-package-test-1"))
	}()
	(&sliptest.Function{
		Source: `(let ((p (make-package 'unlock-package-test-1)))
                  (lock-package p)
                  (unlock-package p)
                  (package-locked-p p))`,
		Expect: `nil`,
	}).Test(t)
}

func TestUnlockPackageSymbol(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("unlock-package-test-2"))
	}()
	(&sliptest.Function{
		Source: `(let ((p (make-package 'unlock-package-test-2)))
                  (lock-package 'unlock-package-test-2)
                  (unlock-package 'unlock-package-test-2)
                  (package-locked-p p))`,
		Expect: `nil`,
	}).Test(t)
}

func TestUnlockPackageString(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("unlock-package-test-3"))
	}()
	(&sliptest.Function{
		Source: `(let ((p (make-package 'unlock-package-test-3)))
                  (lock-package "unlock-package-test-3")
                  (unlock-package "unlock-package-test-3")
                  (package-locked-p p))`,
		Expect: `nil`,
	}).Test(t)
}

func TestUnlockPackageBadPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(unlock-package t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
