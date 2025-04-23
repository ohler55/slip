// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip/sliptest"
)

func TestBitArray(t *testing.T) {
	(&sliptest.Function{
		Source: `(bit (make-array '(2 3) :element-type 'bit :initial-contents '((1 0 1) (0 1 0))) 0 2)`,
		Expect: "1",
	}).Test(t)
}
