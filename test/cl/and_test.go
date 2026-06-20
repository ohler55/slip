// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestAndTrue(t *testing.T) {
	(&sliptest.Function{
		Source: `(and 7)`,
		Expect: "7",
	}).Test(t)
	(&sliptest.Function{
		Source: `(and (+ 1 2) t)`,
		Expect: "t",
	}).Test(t)
	(&sliptest.Function{
		Source: `(and)`,
		Expect: "t",
	}).Test(t)
}

func TestAndFalse(t *testing.T) {
	(&sliptest.Function{
		Source: `(and '())`,
		Expect: "nil",
	}).Test(t)
	(&sliptest.Function{
		Source: `(and t nil)`,
		Expect: "nil",
	}).Test(t)
}

func TestAndPreProv(t *testing.T) {
	orig := slip.Provenance
	slip.Provenance = true
	defer func() { slip.Provenance = orig }()
	(&sliptest.Function{
		Source: `(and (not t) (null nil))`,
		Expect: "nil",
	}).Test(t)
}
