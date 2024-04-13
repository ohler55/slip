// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageName(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-name *cl*)`,
		Expect: `"common-lisp"`,
	}).Test(t)
}

func TestPackageNameNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(package-name t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
