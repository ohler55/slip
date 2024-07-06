// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestRemfSymbol(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((quux '(a 1 b 2 c 3)))
                  (list (remf quux 'b) quux))`,
		Expect: "(t (a 1 c 3))",
	}).Test(t)
	(&sliptest.Function{
		Source: `(let ((quux '(a 1 b 2 c 3)))
                  (list (remf quux 'd) quux))`,
		Expect: "(nil (a 1 b 2 c 3))",
	}).Test(t)
}

func TestRemfPlace(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((quux '(x (a 1 b 2 c 3))))
                  (list (remf (cadr quux) 'b) quux))`,
		Expect: "(t (x (a 1 c 3)))",
	}).Test(t)
}

func TestRemfNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(remf t 'a)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}

func TestRemfNotPlace(t *testing.T) {
	(&sliptest.Function{
		Source:    `(remf '(a 1 b 2 c 3) 'b)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
