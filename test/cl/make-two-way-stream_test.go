// Copyright (c) 2025, Peter Ohler, All rights reserved.

package cl_test

import (
	"testing"

	"github.com/ohler55/slip"
	"github.com/ohler55/slip/sliptest"
)

func TestMakeTwoWayStreamOk(t *testing.T) {
	(&sliptest.Function{
		Source: `(let* ((ss1 (make-string-input-stream "abc"))
                        (ss2 (make-string-output-stream))
                        (tws (make-two-way-stream ss1 ss2))
                        (rr (read tws)))
                  (princ "def" tws)
                  (list rr (get-output-stream-string ss2)))`,
		Expect: `(abc "def")`,
	}).Test(t)
}

func TestMakeTwoWayStreamStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(streamp (make-two-way-stream (make-string-input-stream "x") (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeTwoWayStreamInputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(input-stream-p (make-two-way-stream (make-string-input-stream "x") (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeTwoWayStreamOutputStreamP(t *testing.T) {
	(&sliptest.Function{
		Source: `(output-stream-p (make-two-way-stream (make-string-input-stream "x") (make-string-output-stream)))`,
		Expect: "t",
	}).Test(t)
}

func TestMakeTwoWayStreamNotStream(t *testing.T) {
	(&sliptest.Function{
		Source:    `(make-two-way-stream (make-string-input-stream "abc") t)`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
	(&sliptest.Function{
		Source:    `(make-two-way-stream t (make-string-output-stream))`,
		PanicType: slip.TypeErrorSymbol,
	}).Test(t)
}
