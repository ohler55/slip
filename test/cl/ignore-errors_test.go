// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestIgnoreErrorsCatch(t *testing.T) {
	(&sliptest.Function{
		Source: `(ignore-errors (/ 1 0))`,
		Expect: "/^nil, #<division-by-zero [0-9a-f]+>$/",
	}).Test(t)
}

func TestIgnoreErrorsOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(ignore-errors (+ 1 2))`,
		Expect: "3",
	}).Test(t)
}

func TestIgnoreErrorsPreProv(t *testing.T) {
	orig := slip.Provenance
	slip.Provenance = true
	defer func() { slip.Provenance = orig }()
	(&sliptest.Function{
		Source: `(ignore-errors (+ 1 2) (/ 1 0))`,
		Expect: "/^nil, #<division-by-zero [0-9a-f]+>$/",
	}).Test(t)
}
