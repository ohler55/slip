// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageErrorPackageExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-error-package (make-condition 'package-error :package *package*))`,
		Expect: `#<package common-lisp-user>`,
	}).Test(t)
}

func TestPackageErrorPackageNotPackageError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(package-error-package (make-condition 'error :package *package*))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
