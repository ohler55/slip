// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRempropSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((quux '(a 1 b 2 c 3)))
                  (list (remprop quux 'b) quux))`,
		Expect: "(t (a 1 c 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((quux '(a 1 b 2 c 3)))
                  (list (remprop quux 'd) quux))`,
		Expect: "(nil (a 1 b 2 c 3))",
	}).Test(t)
}

func TestRempropNotSymbol(t *testing.T) {
	(&sliptest.Function{
		Source:    `(remprop t 'a)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRempropNotPlist(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((quux t))
                  (list (remprop quux 'b) quux))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
