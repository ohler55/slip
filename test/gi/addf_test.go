// Copyright (c) 2024, Peter Ohler, All rights reserved.

package gi_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAddfNil(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x nil)) (addf x 1) x)`,
		Expect: "(1)",
	}).Test(t)
}

func TestAddfAppend(t *testing.T) {
	(&sliptest.Function{
		Source: `(let ((x '(1))) (addf x 2 3) x)`,
		Expect: "(1 2 3)",
	}).Test(t)
}

func TestAddfNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(addf t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(let ((x t)) (addf x 1))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
