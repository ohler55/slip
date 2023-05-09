// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFilepathDirectory(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-directory "../one/two/three.lisp")`,
		Expect: `"../one/two"`,
	}).Test(t)
}

func TestFilepathDirectoryBadPath(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-directory t)`,
		Panics: true,
	}).Test(t)
}
