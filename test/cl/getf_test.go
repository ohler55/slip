// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGetfBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(getf '(a 1 b 2 c 3) 'b)`,
		Expect: "2",
	}).Test(t)
}

func TestGetfDefault(t *testing.T) {
	(&sliptest.Function{
		Source: `(getf '(a 1 b 2 c 3) 'd 5)`,
		Expect: "5",
	}).Test(t)
}

func TestGetfNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(getf t 'a)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
