// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeEchoStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream))
                        (es (make-echo-stream ss1 ss2))
                        (echo (read es)))
                  (princ "def" es)
                  (list echo (get-output-stream-string ss2)))`,
		Expect: `(abc "abcdef")`,
	}).Test(t)
}

func TestMakeEchoStreamStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp (make-echo-stream (make-string-input-stream "x") (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeEchoStreamInputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p (make-echo-stream (make-string-input-stream "x") (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeEchoStreamOutputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p (make-echo-stream (make-string-input-stream "x") (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeEchoStreamNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-echo-stream (make-string-input-stream "abc") t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-echo-stream t (make-string-output-stream))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
