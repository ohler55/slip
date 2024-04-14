// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestPackagep(t *testing.T) {
	(&sliptest.Function{
		Source: `(packagep *cl*)`,
		Expect: "t",
	}).Test(t)
}

func TestPackagepNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source: `(packagep t)`,
		Expect: "nil",
	}).Test(t)
}
