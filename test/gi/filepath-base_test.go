// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFilepathBase(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-base "../one/two/three.lisp")`,
		Expect: `"three.lisp"`,
	}).Test(t)
}

func TestFilepathBaseBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-base t)`,
		Panics: true,
	}).Test(t)
}
