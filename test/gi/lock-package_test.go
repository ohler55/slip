// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestLockPackagePackage(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("lock-package-test-1"))
	}()
	(&sliptest.Function{
		Source: `(let ((p (make-package 'lock-package-test-1)))
                  (lock-package p)
                  (package-locked-p p))`,
		Expect: `t`,
	}).Test(t)
}

func TestLockPackageSymbol(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("lock-package-test-2"))
	}()
	(&sliptest.Function{
		Source: `(let ((p (make-package 'lock-package-test-2)))
                  (lock-package 'lock-package-test-2)
                  (package-locked-p p))`,
		Expect: `t`,
	}).Test(t)
}

func TestLockPackageString(t *testing.T) {
	defer func() {
		slip.RemovePackage(slip.FindPackage("lock-package-test-3"))
	}()
	(&sliptest.Function{
		Source: `(let ((p (make-package 'lock-package-test-3)))
                  (lock-package "lock-package-test-3")
                  (package-locked-p p))`,
		Expect: `t`,
	}).Test(t)
}

func TestLockPackageBadPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(lock-package t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
