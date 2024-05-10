// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestPackageNicknames(t *testing.T) {
	(&sliptest.Function{
		Source: `(package-nicknames :cl)`,
		Expect: `("cl")`,
	}).Test(t)
}

func TestPackageNicknamesNotPackage(t *testing.T) {
	(&sliptest.Function{
		Source:    `(package-nicknames t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
