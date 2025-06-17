// Copyright (c) 2025, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGlobList(t *testing.T) {
	(&sliptest.Function{
		Source: `(let (matches)
                  (glob (lambda (x) (addf matches x)) "testdata/*.lisp")
                  matches)`,
		Expect: `("testdata/comp.lisp" "testdata/quux.lisp" "testdata/sys-test.lisp")`,
	}).Test(t)
}

func TestGlobNotString(t *testing.T) {
	(&sliptest.Function{
		Source:    `(glob (lambda (x) (addf result x)) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestGlobBadPattern(t *testing.T) {
	(&sliptest.Function{
		Source:    `(glob (lambda (x) (addf result x)) "][.lisp")`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
