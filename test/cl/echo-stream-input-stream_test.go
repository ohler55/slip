// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestEchoStreamInputStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream)))
                  (equal (echo-stream-input-stream (make-echo-stream ss1 ss2)) ss1))`,
		Expect: "t",
	}).Test(t)
}

func TestEchoStreamInputStreamNotEchoStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(echo-stream-input-stream  t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
