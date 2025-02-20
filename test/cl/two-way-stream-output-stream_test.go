// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTwoWayStreamOutoutStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream)))
                  (equal (two-way-stream-output-stream (make-two-way-stream ss1 ss2)) ss2))`,
		Expect: "t",
	}).Test(t)
}

func TestTwoWayStreamOutoutStreamNotTwoWayStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(two-way-stream-output-stream  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
