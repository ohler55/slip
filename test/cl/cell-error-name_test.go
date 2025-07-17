// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestCellErrorNameExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(cell-error-name (make-condition 'cell-error :name 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestCellErrorNameSub(t *testing.T) {
	(&sliptest.Function{
		Source: `(cell-error-name (make-condition 'unbound-slot :name 'test))`,
		Expect: "test",
	}).Test(t)
}

func TestCellErrorNameNotCellError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(cell-error-name (make-condition 'error :name 'test))`,
		PanicType: slip.ErrorSymbol,
	}).Test(t)
}
