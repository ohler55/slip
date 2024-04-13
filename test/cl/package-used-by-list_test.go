// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageUsedByList(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-used-by-list *gi*)`,
		Expect: "(#<package common-lisp-user>)",
	}).Test(t)
}

func TestPackageUsedByListNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(package-used-by-list t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
