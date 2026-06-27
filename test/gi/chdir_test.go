// Copyright (c) 2026, Peter Ohler, All rights reserved.

package gi_test

import (
	"os"
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestChdirOk(t *testing.T) {
	dir, _ := os.Getwd()
	defer func() { _ = os.Chdir(dir) }()

	(&sliptest.Function{
		Source: `(progn (chdir "..") (getcwd))`,
		Expect: `/slip\/test"$/`,
	}).Test(t)
}

func TestChdirBadPath(t *testing.T) {
	(&sliptest.Function{
		Source:    `(chdir t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestChdirError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(chdir "....")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
