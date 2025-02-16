// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestTwoWayStreamInputStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream)))
                  (equal (two-way-stream-input-stream (make-two-way-stream ss1 ss2)) ss1))`,
		Expect: "t",
	}).Test(t)
}

func TestTwoWayStreamInputStreamNotTwoWayStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(two-way-stream-input-stream  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
