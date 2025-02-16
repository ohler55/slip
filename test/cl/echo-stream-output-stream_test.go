// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEchoStreamOutoutStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream)))
                  (equal (echo-stream-output-stream (make-echo-stream ss1 ss2)) ss2))`,
		Expect: "t",
	}).Test(t)
}

func TestEchoStreamOutoutStreamNotEchoStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(echo-stream-output-stream  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
