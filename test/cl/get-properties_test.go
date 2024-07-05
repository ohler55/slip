// Copyright (c) 2024, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestGetPropertiesBasic(t *testing.T) {
	(&sliptest.Function{
		Source: `(get-properties '(a 1 b 2 c 3) '(b c))`,
		Expect: "b, 2, (b 2 c 3)",
	}).Test(t)
}

func TestGetPropertiesNotFound(t *testing.T) {
	(&sliptest.Function{
		Source: `(get-properties '(a 1 b 2 c 3) '(d e))`,
		Expect: "nil, nil, nil",
	}).Test(t)
}

func TestGetPropertiesNotList(t *testing.T) {
	(&sliptest.Function{
		Source:    `(get-properties t '(a))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(get-properties '(a 1) t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
