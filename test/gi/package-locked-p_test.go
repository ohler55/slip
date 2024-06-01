// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageLockedpTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-locked-p *common-lisp*)`,
		Expect: `t`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(package-locked-p 'common-lisp)`,
		Expect: `t`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(package-locked-p "common-lisp")`,
		Expect: `t`,
	}).Test(t)
}

func TestPackageLockedpFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-locked-p *common-lisp-user*)`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(package-locked-p 'common-lisp-user)`,
		Expect: `nil`,
	}).Test(t)
	(&sliptest.Function{
		Source: `(package-locked-p "common-lisp-user")`,
		Expect: `nil`,
	}).Test(t)
}

func TestPackageLockedpBadPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(package-locked-p t)`,
		PanicType: slip.Symbol("type-error"),
	}).Test(t)
}
