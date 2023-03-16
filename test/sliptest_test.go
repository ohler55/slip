// Copyright (c) 2023, Peter Ohler, All rights reserved.

package test

import (
	"testing"

	"github.com/ohler55/ojg/tt"
	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestSliptestFunctionValidate(t *testing.T) {
	(&sliptest.Function{
		Source: "(setq x 3)",
		Validate: func(t *testing.T, v slip.Object) {
			tt.Equal(t, slip.Fixnum(3), v)
		},
	}).Test(t)
}

func TestSliptestFunctionReadably(t *testing.T) {
	(&sliptest.Function{
		Source:   "12.345e10",
		Expect:   "1.2345d+11",
		Readably: true,
	}).Test(t)
}
