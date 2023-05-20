// Copyright (c) 2023, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestFilepathJoinBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-join "one" "two" "three.lisp")`,
		Expect: `"one/two/three.lisp"`,
	}).Test(t)
}

func TestFilepathJoinEmpty(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-join)`,
		Expect: "nil",
	}).Test(t)
}

func TestFilepathJoinBadElement(t *testing.T) {
	(&sliptest.Function{
		Source: `(filepath-join t)`,
		Panics: true,
	}).Test(t)
}
