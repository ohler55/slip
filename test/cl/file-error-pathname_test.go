// Copyright (c) 2023, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestFileErrorPathnameExact(t *testing.T) {
	(&sliptest.Function{
		Source: `(file-error-pathname (make-condition 'file-error :pathname '(1 2)))`,
		Expect: "(1 2)",
	}).Test(t)
}

func TestFileErrorPathnameNotFileError(t *testing.T) {
	(&sliptest.Function{
		Source:    `(file-error-pathname (make-condition 'error :pathname '(1 2)))`,
		PanicType: slip.Symbol("unbound-slot"),
	}).Test(t)
}
